import os
import re
import subprocess
import sys

RED = '\033[91m'
GREEN = '\033[32m'
NO_COLOR = '\033[0m'

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
            print("%s###%s Compilation should have succeeded, but failed with status %d.\nCode: %s\nOutput: %s"
                  % (RED, NO_COLOR, result.returncode, self.code, result.stderr))
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
            print("%s###%s Expected compilation to fail with message '%s' but it succeeded.\nCode: %s" % 
                (RED, NO_COLOR, self.error, self.code))
            return False
        elif self.error not in str(result.stderr):
            print("%s###%s Compilation error should contain the string '%s' but does not.\nCode: %s\nOutput: %s"
                  % (RED, NO_COLOR, self.error, self.code, result.stderr))
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
    tests = []
    for filename in sys.argv[1:]:
        for test in parse(filename):
            test.module = filename
            tests.append(test)
    tests.reverse()
    passed = 0
    failed = 0
    failed_modules = set()
    for test in tests:
        print(".")
        if test.run():
            passed += 1
        else:
            failed += 1
            failed_modules.add(test.module)
    if failed:
        print("### %d tests %sfailed%s in modules %s (but %s passed)."
            % (failed, RED, NO_COLOR, failed_modules, passed))
        sys.exit(1)
    else:
        print("### %d tests %spassed%s." % (passed, GREEN, NO_COLOR))
