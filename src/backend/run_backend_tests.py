# -*- coding: utf-8 -*-

import subprocess

TESTS = [
    # filename    expected return value
    ("test1.scm", 3),
    ("test2.scm", 3),
    ("test_if_then.scm", 4),
    ("test_if_else.scm", 3),
    ("test_if_1.scm", 4),
]

def main():
    for filename, exp_result in TESTS:
        subprocess.call("gsi ../sins.scm -e '(compile-file \"tests/%s\" \"/tmp/compilation_test.s\")'" %
                        filename, shell=True)
        subprocess.call("gcc -m32 -O0 -g -o /tmp/compilation_test /tmp/compilation_test.s", shell=True)
        result = subprocess.call("/tmp/compilation_test")
        if result == exp_result:
            print "%s: OK" % (filename)
        else:
            print "%s: FAIL (expected: %d, actual: %d)" % (filename, exp_result, result)


if __name__ == '__main__':
    main()
