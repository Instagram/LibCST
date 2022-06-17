import unittest

import libcst as cst


class TestSimpleString(unittest.TestCase):
    def test_quote(self) -> None:
        test_cases = [
            ('"a"', '"'),
            ("'b'", "'"),
            ('""', '"'),
            ("''", "'"),
            ('"""c"""', '"""'),
            ("'''d'''", "'''"),
            ('""""e"""', '"""'),
            ("''''f'''", "'''"),
            ('"""""g"""', '"""'),
            ("'''''h'''", "'''"),
            ('""""""', '"""'),
            ("''''''", "'''"),
        ]

        for s, expected_quote in test_cases:
            simple_string = cst.SimpleString(s)
            actual = simple_string.quote
            self.assertEqual(expected_quote, actual)
