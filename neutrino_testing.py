import os
import re
import subprocess
import sys

def extract_code(md):
    state = "NORMAL"
    code = ""
    lang = ""
    for line in md.splitlines():
        if state == "NORMAL":
            if line == "":
                continue
            if line.startswith("```"):
                state = "CODE"
                lang = line[3:]
            else:
                yield line
        elif state == "CODE":
            if line == "```":
                yield (lang, code)
                state = "NORMAL"
                code = ""
            else:
                code += line + "\n"

class SuccessTest:
    def __init__(self, code, index):
        self.code = code
        self.test_file = "test%d.pl" % index

    def run(self):
        with open(self.test_file, "w") as f:
            f.write(self.code)
        result = subprocess.run(["%s/swipl" % os.getcwd(), "-f", "neutrino.pl", 
                                 "-t", "run('%s')" % self.test_file])
        if result.returncode != 0:
            print("### Compilation should have succeeded, but failed with status %d.\nCode: %s\nOutput: %s"
                  % (result.returncode, self.code, result.stderr))
            return False
        else:
            return True

class FailureTest:
    def __init__(self, code, error, index):
        self.code = code
        self.error = error
        self.test_file = "test%d.pl" % index

    def run(self):
        with open(self.test_file, "w") as f:
            f.write(self.code)
        result = subprocess.run(["%s/swipl" % os.getcwd(), "-f", "neutrino.pl", 
                                 "-t", "run('%s')" % self.test_file], stderr=subprocess.PIPE)
        if result.returncode == 0:
            print("### Expected compilation to fail but it succeeded.\nCode: %s" % self.code)
            return False
        elif self.error not in str(result.stderr):
            print("### Compilation error should contain the string '%s' but does not.\nCode: %s\nOutput: %s"
                  % (self.error, self.code, result.stderr))
            return False
        else:
            return True


def parse(spec_file_name):
    with open(spec_file_name, "r") as f:
        md = f.read()
    state = "NORMAL"
    for index, elem in enumerate(extract_code(md)):
        if state == "NORMAL":
            if isinstance(elem, tuple) and elem[0] == "prolog":
                state = "PROLOG"
                prolog_code = elem[1]
        elif state == "SUCC":
            if isinstance(elem, tuple):
                lang, code = elem
                if lang == "prolog":
                    yield SuccessTest(code, index)
                state = "NORMAL"
            else:
                state = "NORMAL"
        elif state == "PROLOG":
            if isinstance(elem, tuple) and elem[0] == "error":
                yield FailureTest(prolog_code, elem[1].strip(), index)
            state = "NORMAL"
        if isinstance(elem, str) and re.match(".*compile.* successfully.*:$", elem):
            state = "SUCC"

if __name__ == "__main__":
    tests = [test for filename in sys.argv[1:] for test in parse(filename)]
    passed = 0
    failed = 0
    for test in tests:
        if test.run():
            passed += 1
        else:
            failed += 1
    if failed:
        print("### %d tests failed (but %s passed)." % (failed, passed))
        sys.exit(1)
    else:
        print("### %s tests passed." % passed)
