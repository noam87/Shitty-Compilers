# Buggy and incomplete LISP-ish interpreter based mostly on the
# SICP Chapter 4 metacircular
# evaluator with a few changes to better suit Julia implementation.
#
# I also added a simple parser since the host language is not S-expression
# based. The parser generates arrays, which can cause some bugs... maybe
# at some point I'll make the interpreter properly cons-list based
# but I have to go shower now.


module Debug
  export debug
  function debug(msg)
    print_with_color(:blue, "DEBUG>> $(msg)\n")
  end
end

module Parser
  # Simple parser based on Peter Novig's "Lispy" parser.
  # Takes S-expression input
  #
  #   (+ (- 3 1) 4)
  #
  # And returns an AST array:
  #
  #   [:+, [:-, 3, 1], 4]
  #

  using Debug
  export parse

  function parse(program)
    debug("parse($(program))")
    program |> tokenize |> to_ast
  end

  # PRIVATE

  function tokenize(program)
    debug("tokenize($(program))")
    program                   |>
    x->replace(x, "(", " ( ") |>
    x->replace(x, ")", " ) ") |>
    x->replace(x, "\n", " ")  |>
    split
  end

  function to_ast(tokens::Array)
    debug("to_ast($(tokens))")

    if length(tokens) == 0
      error("SYNTAXERR: empty program given")
    end

    token = shift!(tokens)

    if "(" == token
      ast = []

      while tokens[1] != ")"
        push!(ast, to_ast(tokens))
      end

      shift!(tokens) # pop off ")"
      debug("... to_ast() -> $(ast)")
      return ast
    elseif ")" == token
      error("SYNTAXERR unexpected `)`")
    else
      atom(token)
    end
  end

  # Numbers become numbers. Everything else is a symbol.
  function atom(token)
    debug("atom($(token))")
    token                                                                |>
    x -> tryparse(Int, x)                                                |>
    maybeint -> (isnull(maybeint) ? tryparse(Float64, token) : maybeint) |>
    maybenum -> isnull(maybenum) ? Symbol(token) : maybenum.value
  end
end

module Environments
  using Debug
  export Environment, AbstractEnvironment, EmptyEnvironment
  export theglobalenvironment,
         setvariable!,
         definevariable!,
         lookup_variable_value

  # CONSTS

  const PRIMITIVE_PROCEDURES =
    [(:+, +),
     (:-, -),
     (:*, *),
     (:car, x->x[1]),
     (:cdr, x->x[2]),
     (:cons, (x,y)->(debug("push($(x), $(y))"); push!(Array{Any}([x]),y)))] |>
     tups -> map(tup -> (tup[1], [:primitive, tup[2]]), tups) |> # tag procs
     Dict{Symbol, Any}

  # TYPES

  abstract AbstractEnvironment
  type EmptyEnvironment <: AbstractEnvironment end

  type Environment <: AbstractEnvironment
    values::Dict
    outer::AbstractEnvironment
  end

  # FUNCTIONS

  function theglobalenvironment()
    debug("theglobalenvironment()")
    setupenvironment()
  end

  function definevariable!(env::Environment, var::Symbol, val)
    debug("definevariable!(env, $(var), $(val))")
    env.values[var] = val
  end

  function setvariable!(env::Environment, var::Symbol, val)
    debug("setvariable!(env, $(var), $(val))")
    if haskey(env.values, var)
      env.values[var] = val
    else
      setvariable!(env.outer, var, val)
    end
  end

  function setvariable!(env::EmptyEnvironment, var, val)
    debug("setvariable!(env, $(var), $(val))")
    error("ERROR: unbound variable $(var), could not set $(val)")
  end

  function lookup_variable_value(env::Environment, var::Symbol)
    debug("lookup_variable_value($(env), $(var))")
    if haskey(env.values, var)
      env.values[var]
    else
      lookup_variable_value(env.outer, var)
    end
  end

  function lookup_variable_value(env::EmptyEnvironment, var)
    debug("lookup_variable_value(env::EmptyEnvironment, $(var))")
    error("ERROR: unbound variable $(var)")
  end

  # PRIVATE

  function setupenvironment()
    debug("setupenvironment()")
    initialenvironment = Environment(PRIMITIVE_PROCEDURES, EmptyEnvironment())
    definevariable!(initialenvironment, :t, true)
    definevariable!(initialenvironment, :f, false)
    initialenvironment
  end
end

module Interpreter
  using Debug
  using Environments
  import Parser
  export eval

  # FUNCTIONS

  function eval(expression)
    eval(expression, theglobalenvironment())
  end

  function eval(expression::String, environment)
    Parser.parse(expression) |>
    parsed -> eval(parsed, environment)
  end

  function eval(expression::Array, environment)
    analyze(expression)(environment)
  end

  # PRIVATE

  function analyze(exp)
    debug("analyze($(exp))")
    if is_self_evaluating(exp)
      debug("analyze_self_evaluating($(exp))")
      env -> exp
    elseif is_variable(exp)
      debug("analyze_variable($(exp))")
      env -> lookup_variable_value(env, exp)
    # '(quote text)
    elseif is_tagged_list(exp, :quote)
      debug("analyze_quote($(exp))")
      env -> exp[2]                      # TODO: should be a cons list
    elseif is_tagged_list(exp, :set!)
      analyze_assignment(exp)
    elseif is_tagged_list(exp, :define)
      analyze_definition(exp)
    elseif is_tagged_list(exp, :if)
      analyze_if(exp)
    elseif is_tagged_list(exp, :lambda)
      analyze_lambda(exp)
    elseif is_tagged_list(exp, :begin)
      analyze_sequence(exp[2:end])
    elseif isa(exp, Array)
      analyze_application(exp)
    else
      error("SYNTAXERR unknown expression type $(exp)")
    end
  end

  function is_self_evaluating(exp)
    debug("is_self_evaluating($(exp))")
    (isa(exp, Number) || isa(exp, String)) ? true : false
  end

  function is_tagged_list(exp, tag::Symbol)
    debug("is_tagged_list($(exp), $(tag))")
    exp[1] == tag
  end

  function is_variable(exp)
    debug("is_variable($(exp))")
    isa(exp, Symbol)
  end

  function analyze_sequence(exps)
    debug("analyze_sequence($(exps))")
    sequentially = (proc1, proc2) -> (env) -> (proc1(env); proc2(env))
    procs = map(analyze, exps)

    function loop(procs)
      debug("... loop($(procs))")
      if length(procs) == 1
        procs[1]
      else
        loop(sequentially(procs[1], procs[2]), procs[3:end])
      end
    end

    if length(procs) == 0
      error("ANALYZEERR empty sequence")
    else
      loop(procs)
    end
  end

  # (set! varname value)
  function analyze_assignment(exp)
    debug("analyze_assignment($(exp))")
    varname = exp[2]
    # If the value is also a compound expression, return lambda that will
    # evaluate it.
    valueproc = analyze(exp[3])
    env -> (setvariable!(env, varname, valueproc(env)); :ok)
  end

  function analyze_definition(exp)
    debug("analyze_definition($(exp))")
    defname = definitionvariable(exp)
    # Similar to analyze_assignment
    valueproc = definitionvalue(exp) |> analyze
    env -> (definevariable!(env, defname, valueproc(env)); :ok)
  end

  # (if predicate consequent alternative)
  function analyze_if(exp)
    debug("analyze_if($(exp))")
    predicateproc = analyze(exp[2])
    consequentproc = analyze(exp[3])
    alternativeproc = analyze(exp[4])
    env -> predicateproc(env) ? consequentproc(env) : alternativeproc(env)
  end

  # (lambda parameters body)
  function analyze_lambda(exp)
    debug("analyze_lambda($(exp))")
    args = exp[2]
    bodyproc = analyze_sequence(exp[3:end])
    # PROC
    env -> [:procedure, args, bodyproc, env]
  end

  # (operator operands)
  function analyze_application(exp)
    debug("analyze_application($(exp))")
    fproc = analyze(exp[1])
    aprocs = map(analyze, exp[2:end])
    env -> execute_application(fproc(env), map(aproc->aproc(env), aprocs))
  end

  function execute_application(proc, args)
    debug("execute_application($(proc), $(args))")
    if is_tagged_list(proc, :primitive)
      debug("... $(proc[1])($(args))")
      proc[2](args...) # proc = [:primitive, lambda]
    elseif is_tagged_list(proc, :procedure)
      # See PROC
      args_dict = Dict(zip(proc[2], args))
      new_env = Environment(args_dict, proc[4]) # Extend env and pass to proc.
      proc[3](new_env)
    else
      error("ERR unknown procedure $(proc)")
    end
  end

  function definitionvariable(exp)
    debug("definitionvariable($(exp))")
    if isa(exp[2], Symbol)
      exp[2]               # (define VAR value)
    else
      exp[2][1]            # (define (VAR args) lambda)
    end
  end

  function definitionvalue(exp)
    debug("definitionvalue($(exp))")
    if isa(exp[2], Symbol)
      exp[3]
    else
      makelambda(exp[2][2:end], exp[3])
    end
  end

  function makelambda(args, body)
    debug("makelambda($(args), $(body))")
    [:lambda, args, body]
  end
end

module REPL
  using Debug
  using Interpreter
  using Environments
  using Parser
  export run

  # CONSTS

  const  INPUT_PROMPT = "--- INPUT ---"
  const  OUTPUT_PROMPT = "--- RESULT ---"

  # FNUCTIONS

  function run()
    printforio(INPUT_PROMPT)
    input = readline(STDIN) |> Parser.parse
    output = Interpreter.eval(input, theglobalenvironment())
    printforio(OUTPUT_PROMPT)
    printresult(output)
    run()
  end

  # PRIVATE

  function printforio(text)
    print_with_color(:yellow, "\n$(text)\n")
  end

  function printresult(text)
    print_with_color(:red, "$(text)")
  end
end

REPL.run()
