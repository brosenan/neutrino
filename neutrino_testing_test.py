import neutrino_testing
import unittest
from unittest import mock
import subprocess

class TestTesting(unittest.TestCase):
    def test_extract_code(self):
        md = """
One
Two
Three
```foo
uno
dos
tres
```
Four
"""
        self.assertSequenceEqual(list(neutrino_testing.extract_code(md)), [
            "One", "Two", "Three", ("foo", "uno\ndos\ntres\n"), "Four"])

    def test_parse_doc(self):
        tests = list(neutrino_testing.parse("docspec.md"))
        self.assertEqual(len(tests), 1)
        self.assertTrue(isinstance(tests[0], neutrino_testing.SuccessTest))
        self.assertEqual(tests[0].code, "% Some Neutrino code that must compile.\n")

    @mock.patch('subprocess.run')
    def test_successful_compilation(self, run_mock):
        run_mock.return_value = subprocess.CompletedProcess([], returncode=0)


if __name__ == "__main__":
    unittest.main()
