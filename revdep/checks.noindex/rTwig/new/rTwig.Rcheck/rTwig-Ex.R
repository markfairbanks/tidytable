pkgname <- "rTwig"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rTwig')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("box_dimension")
### * box_dimension

flush(stderr()); flush(stdout())

### Name: box_dimension
### Title: Box Dimension
### Aliases: box_dimension

### ** Examples

## Calculate Box Dimension
file <- system.file("extdata/cloud.txt", package = "rTwig")
cloud <- read.table(file, header = FALSE)
output <- box_dimension(cloud, plot = "ALL")
output




cleanEx()
nameEx("correct_radii")
### * correct_radii

flush(stderr()); flush(stdout())

### Name: correct_radii
### Title: Correct Radii
### Aliases: correct_radii

### ** Examples





cleanEx()
nameEx("export_mat")
### * export_mat

flush(stderr()); flush(stdout())

### Name: export_mat
### Title: Export MAT
### Aliases: export_mat

### ** Examples





cleanEx()
nameEx("export_mesh")
### * export_mesh

flush(stderr()); flush(stdout())

### Name: export_mesh
### Title: Export Mesh
### Aliases: export_mesh

### ** Examples





cleanEx()
nameEx("import_qsm")
### * import_qsm

flush(stderr()); flush(stdout())

### Name: import_qsm
### Title: Import QSM
### Aliases: import_qsm

### ** Examples


## Read a TreeQSM MATLAB file in the 2.3.x - 2.4.x format
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_qsm(file, version = "2.x.x")
summary(qsm)

## Read a TreeQSM MATLAB file in the 2.0 format
file <- system.file("extdata/QSM_2.mat", package = "rTwig")
qsm <- import_qsm(file, version = "2.0")
names(qsm)




cleanEx()
nameEx("plot_qsm")
### * plot_qsm

flush(stderr()); flush(stdout())

### Name: plot_qsm
### Title: Plot QSM
### Aliases: plot_qsm

### ** Examples





cleanEx()
nameEx("plot_stand")
### * plot_stand

flush(stderr()); flush(stdout())

### Name: plot_stand
### Title: Plot Stand
### Aliases: plot_stand

### ** Examples





cleanEx()
nameEx("qsm_summary")
### * qsm_summary

flush(stderr()); flush(stdout())

### Name: qsm_summary
### Title: QSM Summary
### Aliases: qsm_summary

### ** Examples


## TreeQSM Processing Chain
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_qsm(file)
cylinder <- qsm$cylinder
cylinder <- update_cylinders(cylinder)
qsm_summary(cylinder)

# TreeQSM Triangulation
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_qsm(file)
cylinder <- qsm$cylinder
cylinder <- update_cylinders(cylinder)
triangulation <- qsm$triangulation
qsm_summary(cylinder = cylinder, triangulation = triangulation)

## SimpleForest Processing Chain
file <- system.file("extdata/QSM.csv", package = "rTwig")
cylinder <- read.csv(file)
cylinder <- update_cylinders(cylinder)
qsm_summary(cylinder)




cleanEx()
nameEx("smooth_qsm")
### * smooth_qsm

flush(stderr()); flush(stdout())

### Name: smooth_qsm
### Title: Smooth QSM
### Aliases: smooth_qsm

### ** Examples





cleanEx()
nameEx("update_cylinders")
### * update_cylinders

flush(stderr()); flush(stdout())

### Name: update_cylinders
### Title: Update Cylinders
### Aliases: update_cylinders

### ** Examples


## TreeQSM Processing Chain
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_qsm(file)
cylinder <- qsm$cylinder
cylinder <- update_cylinders(cylinder)
str(cylinder)

## SimpleForest Processing Chain
file <- system.file("extdata/QSM.csv", package = "rTwig")
cylinder <- read.csv(file)
cylinder <- update_cylinders(cylinder)
str(cylinder)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
