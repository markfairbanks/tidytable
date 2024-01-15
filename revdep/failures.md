# bbknnR

<details>

* Version: 1.1.0
* GitHub: https://github.com/ycli1995/bbknnR
* Source code: https://github.com/cran/bbknnR
* Date/Publication: 2023-11-20 15:10:09 UTC
* Number of recursive dependencies: 163

Run `revdepcheck::revdep_details(, "bbknnR")` for more info

</details>

## Newly broken

*   checking whether package ‘bbknnR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/xmxf129/MyDocs/R/Packages/tidytable/revdep/checks.noindex/bbknnR/new/bbknnR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bbknnR’ ...
** package ‘bbknnR’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.1.0.2.5)’
using SDK: ‘MacOSX14.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/xmxf129/MyDocs/R/Packages/tidytable/revdep/library.noindex/bbknnR/Rcpp/include' -I/opt/R/arm64/include -I/opt/homebrew/opt/libomp/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/xmxf129/MyDocs/R/Packages/tidytable/revdep/library.noindex/bbknnR/Rcpp/include' -I/opt/R/arm64/include -I/opt/homebrew/opt/libomp/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -c data_manipulation.cpp -o data_manipulation.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/opt/libomp/lib -lomp -o bbknnR.so RcppExports.o data_manipulation.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: -single_module is obsolete
...
installing to /Users/xmxf129/MyDocs/R/Packages/tidytable/revdep/checks.noindex/bbknnR/new/bbknnR.Rcheck/00LOCK-bbknnR/00new/bbknnR/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘get_dummies.’ is not exported by 'namespace:tidytable'
Execution halted
ERROR: lazy loading failed for package ‘bbknnR’
* removing ‘/Users/xmxf129/MyDocs/R/Packages/tidytable/revdep/checks.noindex/bbknnR/new/bbknnR.Rcheck/bbknnR’


```
### CRAN

```
* installing *source* package ‘bbknnR’ ...
** package ‘bbknnR’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.1.0.2.5)’
using SDK: ‘MacOSX14.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/xmxf129/MyDocs/R/Packages/tidytable/revdep/library.noindex/bbknnR/Rcpp/include' -I/opt/R/arm64/include -I/opt/homebrew/opt/libomp/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/xmxf129/MyDocs/R/Packages/tidytable/revdep/library.noindex/bbknnR/Rcpp/include' -I/opt/R/arm64/include -I/opt/homebrew/opt/libomp/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -c data_manipulation.cpp -o data_manipulation.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/opt/libomp/lib -lomp -o bbknnR.so RcppExports.o data_manipulation.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: -single_module is obsolete
...
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (bbknnR)


```
# journalabbr

<details>

* Version: 0.4.0
* GitHub: https://github.com/zoushucai/journalabbr
* Source code: https://github.com/cran/journalabbr
* Date/Publication: 2022-08-09 16:00:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::revdep_details(, "journalabbr")` for more info

</details>

## Newly broken

*   checking whether package ‘journalabbr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/xmxf129/MyDocs/R/Packages/tidytable/revdep/checks.noindex/journalabbr/new/journalabbr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘journalabbr’ ...
** package ‘journalabbr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘enframe.’ is not exported by 'namespace:tidytable'
Execution halted
ERROR: lazy loading failed for package ‘journalabbr’
* removing ‘/Users/xmxf129/MyDocs/R/Packages/tidytable/revdep/checks.noindex/journalabbr/new/journalabbr.Rcheck/journalabbr’


```
### CRAN

```
* installing *source* package ‘journalabbr’ ...
** package ‘journalabbr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (journalabbr)


```
