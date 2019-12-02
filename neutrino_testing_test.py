import neutrino_testing
import os
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
        self.assertEqual(len(tests), 2)
        self.assertTrue(isinstance(tests[0], neutrino_testing.SuccessTest))
        self.assertEqual(tests[0].code, "% Some Neutrino code that must compile.\n")
        self.assertTrue(isinstance(tests[1], neutrino_testing.FailureTest))
        self.assertEqual(tests[1].code, "% This code should fail to compile.\n")
        self.assertEqual(tests[1].error, "This text must appear in the error.")

    @mock.patch('subprocess.run')
    def test_successful_compilation(self, run_mock):
        run_mock.return_value = subprocess.CompletedProcess([], returncode=0)
        code = "Some code..."
        test = neutrino_testing.SuccessTest(code, 4)
        self.assertTrue(test.run())
        with open("test4.pl") as f:
            self.assertEqual(f.read(), code)
        run_mock.assert_called_with(["%s/swipl" % os.getcwd(), "-f", "neutrino.pl", 
                                     "-t", "run('test4.pl')"])

    @mock.patch('subprocess.run')
    def test_successful_compilation_failure(self, run_mock):
        run_mock.return_value = subprocess.CompletedProcess([], returncode=2)
        code = "Some code..."
        test = neutrino_testing.SuccessTest(code, 5)
        self.assertFalse(test.run())

    @mock.patch('subprocess.run')
    def test_failed_compilation(self, run_mock):
        run_mock.return_value = subprocess.CompletedProcess([], returncode=1,
                                                            stderr="Failure text...")
        code = "Some more code..."
        error = "Failure text"
        test = neutrino_testing.FailureTest(code, error, 7)
        self.assertTrue(test.run())
        with open("test7.pl") as f:
            self.assertEqual(f.read(), code)
        run_mock.assert_called_with(["%s/swipl" % os.getcwd(), "-f", "neutrino.pl", 
                                     "-t", "run('test7.pl')"], stderr=subprocess.PIPE)

    @mock.patch('subprocess.run')
    def test_failed_compilation_fails_due_to_successful_compilation(self, run_mock):
        run_mock.return_value = subprocess.CompletedProcess([], returncode=0)
        code = "Some more code..."
        error = "Failure text"
        test = neutrino_testing.FailureTest(code, error, 6)
        self.assertFalse(test.run())

    @mock.patch('subprocess.run')
    def test_failed_compilation_fails_due_to_wrong_error(self, run_mock):
        run_mock.return_value = subprocess.CompletedProcess([], returncode=2,
                                                            stderr="Wrong error")
        code = "Some more code..."
        error = "Failure text"
        test = neutrino_testing.FailureTest(code, error, 8)
        self.assertFalse(test.run())

if __name__ == "__main__":
    unittest.main()
