## Submit release

* Check and fix any errors at https://cran.r-project.org/web/checks/check_results_quickblock.html
* Delete content of `NAMESPACE` and run `document()`
* Run `load_all(recompile = TRUE)`, `test()` and `check()`
* Run `build_win(version = "R-release")` and `build_win(version = "R-devel")`
* Run `revdep_check()`, remove `revdep` folder when done
* Update package information
	- Set new version number in `DESCRIPTION`
	- Set release date in `DESCRIPTION`
	- Change "quickblock devel" to "quickblock VERSION" in `NEWS.md`
	- Update `cran-comments.md` with correct information
	- Update travis and appveyor with current versions
* Commit and push to github so automatic tests run
* Run `load_all(recompile = TRUE)`, `test()` and `check()`
* Run `build_win(version = "R-release")` and `build_win(version = "R-devel")`
* Run `revdep_check()`, remove revdep folder when done
* Wait until all tests are done
* Submit to CRAN
	- Run `build()`
	- Upload to http://cran.r-project.org/submit.html
	- Add `cran-comments.md` as comment


## When accepted

* Add new release to Github
	- Add CRAN release as binary
	- Add relevant information from `NEWS.md`
* Update package information
	- Add .9000 to the version number in `DESCRIPTION`
	- Set date in `DESCRIPTION`
	- Add "quickblock devel" to `NEWS.md`
	- Commit and push to github
