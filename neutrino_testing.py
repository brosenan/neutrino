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
    def __init__(self, code):
        self.code = code
    def run(self):
        with open("test.pl", "w") as f:
            f.write(self.code)
        result = subprocess.run(["%s/swipl" % os.getcwd(), "-f", "neutrino.pl", 
                                 "-t", "run('test.pl')"])
        if result.returncode != 0:
            print("### Compilation should have succeeded, but failed with status %d.\nCode: %s\nOutput: %s"
                  % (result.returncode, self.code, result.stderr))
            return False
        else:
            return True

class FailureTest:
    def __init__(self, code, error):
        self.code = code
        self.error = error

    def run(self):
        with open("test.pl", "w") as f:
            f.write(self.code)
        result = subprocess.run(["%s/swipl" % os.getcwd(), "-f", "neutrino.pl", 
                                 "-t", "run('test.pl')"], stderr=subprocess.PIPE)
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
    for elem in extract_code(md):
        if state == "NORMAL":
            if isinstance(elem, tuple) and elem[0] == "prolog":
                state = "PROLOG"
                prolog_code = elem[1]
        elif state == "SUCC":
            if isinstance(elem, tuple):
                lang, code = elem
                if lang == "prolog":
                    yield SuccessTest(code)
                state = "NORMAL"
            else:
                state = "NORMAL"
        elif state == "PROLOG":
            if isinstance(elem, tuple) and elem[0] == "error":
                yield FailureTest(prolog_code, elem[1].strip())
            state = "NORMAL"
        if isinstance(elem, str) and re.match(".*compile.* successfully.*:$", elem):
            state = "SUCC"

if __name__ == "__main__":
    tests = list(parse(sys.argv[1]))
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
