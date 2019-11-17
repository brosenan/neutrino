import re
import subprocess

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
        result = subprocess.run(["./swipl", "-f", "neutrino.pl", 
                                 "-t", "run('test.pl')"])
        return result.returncode == 0

class FailureTest:
    def __init__(self, code, error):
        self.code = code
        self.error = error

    def run(self):
        with open("test.pl", "w") as f:
            f.write(self.code)
        result = subprocess.run(["./swipl", "-f", "neutrino.pl", 
                                 "-t", "run('test.pl')"], stderr=subprocess.PIPE)
        return result.returncode != 0 and self.error in result.stderr

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
        if isinstance(elem, str) and re.match(".*compiles successfully.*:$", elem):
            state = "SUCC"

