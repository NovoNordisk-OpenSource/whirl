# mypy: disable-error-code="operator"
1 + "a"  # pyright: ignore [reportOperatorIssue, reportUnusedExpression]

raise TypeError("This is a type error for testing purposes")

raise Exception("Error also for testing")
