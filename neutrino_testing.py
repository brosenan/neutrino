import re

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

def parse(spec_file_name):
    with open(spec_file_name, "r") as f:
        md = f.read()
    state = "NORMAL"
    for elem in extract_code(md):
        if state == "NORMAL":
            if isinstance(elem, str) and re.match(".*compiles successfully.*:$", elem):
                state = "SUCC"
        elif state == "SUCC":
            if isinstance(elem, tuple):
                lang, code = elem
                if lang == "prolog":
                    yield SuccessTest(code)
                state = "NORMAL"
            else:
                state = "NORMAL"
