import sys
sys.setrecursionlimit(1000000)

import syntax
import parsing
import typechecker
import evaluator_dfs

def run(filename):
    with open(filename) as f:
        source = f.read()

    parser = parsing.Parser(source, filename=filename)
    ast = parser.parse_program()
    #print(ast.show())

    typechecker_ = typechecker.TypeChecker()
    checked_ast = typechecker_.check_program(ast)
    #print(checked_ast.show())

    evaluator = evaluator_dfs.Evaluator()
    results = evaluator.eval_program(checked_ast, strategy='strong')
    for result in results:
        print(result.show())
        input(" ; ")
    print("done.")

def usage(program):
    sys.stderr.write('Usage: {program} input.fa\n'.format(program=program))
    sys.exit()

def main(argv):
    if len(argv) == 2:
        run(argv[1])
    else:
        usage(argv[0])

if __name__ == '__main__':
    main(sys.argv)

