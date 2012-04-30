# -*- coding: utf-8 -*-

import subprocess

TESTS = [
    # filename    expected return value
    # ("test1.scm"             , 3<<2),
    # ("test2.scm"             , 3<<2),
    # ("test_if_then.scm"      , 4<<2),
    # ("test_if_else.scm"      , 3<<2),
    # ("test_if_1.scm"         , 4<<2),
    # ("test_prim.scm"         , 7<<2),
    # ("test_let1.scm"         , 3<<2),
    # ("test_let2.scm"         , 7<<2),
    # ("test_let3.scm"         , 7<<2),
    # ("test_letstar.scm"      , 7<<2),
    # ("test_set.scm"          , 4<<2),
    # ("test_define.scm"       , 3<<2),
    # ("test_define2.scm"      , 3<<2),
    # ("test_func.scm"         , 3<<2),
    ("test_func2.scm"        , 12<<2),
    # ("test_func3.scm"        , 6<<2),
    # ("test_rec.scm"          , 42<<2),
    # ("test_rec2.scm"         , 5<<2),
    # ("test_let_lambda_1.scm" , 7<<2),
    # ("test_let_lambda_2.scm" , 7<<2),
    # ("test_fib.scm"          , 13<<2),
    # ("test_fact.scm"         , 24<<2),
    # ("test_loop1.scm"        , 5<<2),
]

def main():
    for filename, exp_result in TESTS:
        subprocess.call("gsi ../sins.scm -e '(compile-file \"tests/%s\" \"/tmp/compilation_test.s\")'" %
                        filename, shell=True)
#        subprocess.call("gcc -m32 -O0 -g -o /tmp/compilation_test *.o /tmp/compilation_test.s", shell=True)
        subprocess.call("gcc -O0 -g -o /tmp/compilation_test *.o /tmp/compilation_test.s", shell=True)
        result = subprocess.call("/tmp/compilation_test")
        if result == exp_result:
            print "%s: OK" % (filename)
        else:
            print "%s: FAIL (expected: %d, actual: %d)" % (filename, exp_result, result)


if __name__ == '__main__':
    main()
