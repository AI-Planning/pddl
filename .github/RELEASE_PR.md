## Release summary

Version number: [e.g. 1.0.1]

## Release details

Describe in short the main changes with the new release.

## Checklist

_Put an `x` in the boxes that apply._

- [ ] I have read the [CONTRIBUTING](../main/CONTRIBUTING.rst) doc
- [ ] I am making a pull request against the `main` branch
- [ ] I've updated the dependencies versions in `Pipfile` to the latest, wherever is possible.
- [ ] Lint and unit tests pass locally (please run tests also manually, not only with `tox`)
- [ ] I built the documentation and updated it with the latest changes
- [ ] I've added an item in `CHANGES.md` for this release
- [ ] I bumped the version number in the `__version__.py` file.
- [ ] I published the latest version on TestPyPI and checked that the following command work:
       ```pip install pddl==<version-number> --index-url https://test.pypi.org/simple --force --no-cache-dir --no-deps```
- [ ] After merging the PR, I'll publish the build also on PyPI. Then, I'll make sure the following
      command will work:
      ```pip install pddl==<version_number> --force --no-cache-dir --no-deps```  
- [ ] After merging the PR, I'll tag the repo with `v${VERSION_NUMVER}` (e.g. `v0.1.2`)


## Further comments

Write here any other comment about the release, if any.
