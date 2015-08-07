A practical tutorial on S4 programming
======================================

## Experiment
Say we have conducted a microarray experiment using `n` probes across `m`
individuals.

* We can represent the results of this experiment using a matrix
  of size `n` x `m` called `marray`. This matrix stores the intensity for
  probe `1, 2, ..., n` for sample `1, 2, ..., m`.
* The sample annotation can be described in a data.frame `samp_annot`
  with `m` rows and any number of columns.
* The probe annotation can be described in a data.frame
  `prob_annot` with `n` rows and any number of columns.



```r
set.seed(42)
n <- 10
m <- 6
intensities <- rnorm(n * m, mean = 10, sd = 5)
marray <- matrix(intensities, ncol = m)
marray
```

```
##            [,1]      [,2]      [,3]      [,4]      [,5]      [,6]
##  [1,] 16.854792 16.524348  8.466807 12.277251 11.029993 11.609626
##  [2,]  7.176509 21.433227  1.093458 13.524187  8.194714  6.080805
##  [3,] 11.815642  3.055696  9.140413 15.175518 13.790816 17.878638
##  [4,] 13.164313  8.606056 16.073373  6.955368  6.366476 13.214497
##  [5,] 12.021342  9.333393 19.475967 12.524776  3.158595 10.448803
##  [6,]  9.469377 13.179752  7.847654  1.414957 12.164090 11.382754
##  [7,] 17.557610  8.578735  8.713653  6.077705  5.943034 13.396444
##  [8,]  9.526705 -3.282277  1.184185  5.745462 17.220506 10.449164
##  [9,] 20.092119 -2.202335 12.300487 -2.071038  7.842769 -4.965450
## [10,]  9.686430 16.600567  6.800026 10.180613 13.278239 11.424415
```

```r
# sample annotation
samp_annot <- data.frame(sampleID = 1:m,
			 condition = rep(c("WT", "MUT"), each = 3))
rownames(samp_annot) <- colnames(marray) <- LETTERS[1:m]
samp_annot
```

```
##   sampleID condition
## A        1        WT
## B        2        WT
## C        3        WT
## D        4       MUT
## E        5       MUT
## F        6       MUT
```

```r
# probe annotation
prob_annot <- data.frame(geneID = 1:n,
			  pathway = sample(LETTERS, n, replace = TRUE))
rownames(prob_annot) <- rownames(marray) <- paste0("probe", 1:n)
prob_annot
```

```
##         geneID pathway
## probe1       1       J
## probe2       2       K
## probe3       3       O
## probe4       4       P
## probe5       5       S
## probe6       6       K
## probe7       7       X
## probe8       8       Z
## probe9       9       G
## probe10     10       S
```

```r
marray
```

```
##                 A         B         C         D         E         F
## probe1  16.854792 16.524348  8.466807 12.277251 11.029993 11.609626
## probe2   7.176509 21.433227  1.093458 13.524187  8.194714  6.080805
## probe3  11.815642  3.055696  9.140413 15.175518 13.790816 17.878638
## probe4  13.164313  8.606056 16.073373  6.955368  6.366476 13.214497
## probe5  12.021342  9.333393 19.475967 12.524776  3.158595 10.448803
## probe6   9.469377 13.179752  7.847654  1.414957 12.164090 11.382754
## probe7  17.557610  8.578735  8.713653  6.077705  5.943034 13.396444
## probe8   9.526705 -3.282277  1.184185  5.745462 17.220506 10.449164
## probe9  20.092119 -2.202335 12.300487 -2.071038  7.842769 -4.965450
## probe10  9.686430 16.600567  6.800026 10.180613 13.278239 11.424415
```

Now let's add these three pieces to a list:


```r
maexp <- list(marray = marray,
	      samp_annot = samp_annot,
	      prob_annot = prob_annot)
rm(marray, samp_annot, prob_annot, intensities)
str(maexp)
```

```
## List of 3
##  $ marray    : num [1:10, 1:6] 16.85 7.18 11.82 13.16 12.02 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:10] "probe1" "probe2" "probe3" "probe4" ...
##   .. ..$ : chr [1:6] "A" "B" "C" "D" ...
##  $ samp_annot:'data.frame':	6 obs. of  2 variables:
##   ..$ sampleID : int [1:6] 1 2 3 4 5 6
##   ..$ condition: Factor w/ 2 levels "MUT","WT": 2 2 2 1 1 1
##  $ prob_annot:'data.frame':	10 obs. of  2 variables:
##   ..$ geneID : int [1:10] 1 2 3 4 5 6 7 8 9 10
##   ..$ pathway: Factor w/ 8 levels "G","J","K","O",..: 2 3 4 5 6 3 7 8 1 6
```

```r
ls()
```

```
## [1] "m"     "maexp" "n"
```

Let's access and manipulate some of the elements of the `maexp` list:


```r
maexp$samp_annot # the whole data.frame
```

```
##   sampleID condition
## A        1        WT
## B        2        WT
## C        3        WT
## D        4       MUT
## E        5       MUT
## F        6       MUT
```

```r
summary(maexp$marray[, "A"]) # the A sample
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   7.177   9.567  11.920  12.740  15.930  20.090
```

```r
(wt <- maexp$samp_annot[, "condition"] == "WT") # WT samples
```

```
## [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE
```

```r
maexp$marray["probe8", wt] # probe8 for WT samples
```

```
##         A         B         C 
##  9.526705 -3.282277  1.184185
```

```r
maexp[["marray"]]["probe3", !wt] # probe3 for MUT samples
```

```
##        D        E        F 
## 15.17552 13.79082 17.87864
```


```r
boxplot(maexp$marray,
	main = paste0("Boxplot representing the intensity\n",
		      "distributions of the 10 probes for the",
		      " 6 samples"),
	xlab = "Sample", ylab = "Probe Intensity")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Now let's extract the first 10 probes for the first 3 samples:


```r
x <- 1:5
y <- 1:3
marray2 <- maexp$marray[x, y]
prob_annot2 <- maexp$prob_annot[x, ]
samp_annot2 <- maexp$samp_annot[y, ]
maexp2 <- list(marray = marray2,
	       samp_annot = samp_annot2,
	       prob_annot = prob_annot2)
rm(marray2, samp_annot2, prob_annot2)
str(maexp2)
```

```
## List of 3
##  $ marray    : num [1:5, 1:3] 16.85 7.18 11.82 13.16 12.02 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:5] "probe1" "probe2" "probe3" "probe4" ...
##   .. ..$ : chr [1:3] "A" "B" "C"
##  $ samp_annot:'data.frame':	3 obs. of  2 variables:
##   ..$ sampleID : int [1:3] 1 2 3
##   ..$ condition: Factor w/ 2 levels "MUT","WT": 2 2 2
##  $ prob_annot:'data.frame':	5 obs. of  2 variables:
##   ..$ geneID : int [1:5] 1 2 3 4 5
##   ..$ pathway: Factor w/ 8 levels "G","J","K","O",..: 2 3 4 5 6
```

As you can see it's a bit cumbersome and error-prone. There's a much better way
to manipulate "objects" like microarrays.


## Class Definition
We can define a class with the `setClass` function. It requires a name
(`MArray`) and a content structure. The different elements of an S4 class are
called slots. When defining the slots, we provide their names and classes as a
named vector or list. You need to create objects with exactly these types of
slots.


```r
MArray <- setClass("MArray",
		   slots = c(marray = "matrix",
			     samp_annot = "data.frame",
			     prob_annot = "data.frame"))
```

The `setClass` function returns a constructor that can be used to create an
instance (object) of the MArray class.



```r
MArray() # an empty object
```

```
## An object of class "MArray"
## Slot "marray":
## <0 x 0 matrix>
## 
## Slot "samp_annot":
## data frame with 0 columns and 0 rows
## 
## Slot "prob_annot":
## data frame with 0 columns and 0 rows
```

```r
# MArray(marray = 1:2) # not allowed since should be matrix
MArray(marray = matrix(1:2)) # now allowed
```

```
## An object of class "MArray"
## Slot "marray":
##      [,1]
## [1,]    1
## [2,]    2
## 
## Slot "samp_annot":
## data frame with 0 columns and 0 rows
## 
## Slot "prob_annot":
## data frame with 0 columns and 0 rows
```

```r
ma <- MArray(marray = maexp$marray,
	     samp_annot = maexp$samp_annot,
	     prob_annot = maexp$prob_annot
	     )
class(ma)
```

```
## [1] "MArray"
## attr(,"package")
## [1] ".GlobalEnv"
```

```r
ma
```

```
## An object of class "MArray"
## Slot "marray":
##                 A         B         C         D         E         F
## probe1  16.854792 16.524348  8.466807 12.277251 11.029993 11.609626
## probe2   7.176509 21.433227  1.093458 13.524187  8.194714  6.080805
## probe3  11.815642  3.055696  9.140413 15.175518 13.790816 17.878638
## probe4  13.164313  8.606056 16.073373  6.955368  6.366476 13.214497
## probe5  12.021342  9.333393 19.475967 12.524776  3.158595 10.448803
## probe6   9.469377 13.179752  7.847654  1.414957 12.164090 11.382754
## probe7  17.557610  8.578735  8.713653  6.077705  5.943034 13.396444
## probe8   9.526705 -3.282277  1.184185  5.745462 17.220506 10.449164
## probe9  20.092119 -2.202335 12.300487 -2.071038  7.842769 -4.965450
## probe10  9.686430 16.600567  6.800026 10.180613 13.278239 11.424415
## 
## Slot "samp_annot":
##   sampleID condition
## A        1        WT
## B        2        WT
## C        3        WT
## D        4       MUT
## E        5       MUT
## F        6       MUT
## 
## Slot "prob_annot":
##         geneID pathway
## probe1       1       J
## probe2       2       K
## probe3       3       O
## probe4       4       P
## probe5       5       S
## probe6       6       K
## probe7       7       X
## probe8       8       Z
## probe9       9       G
## probe10     10       S
```

We can access individual slots from an object using `@` in a similar manner to
`$` with lists:



```r
ma@samp_annot
```

```
##   sampleID condition
## A        1        WT
## B        2        WT
## C        3        WT
## D        4       MUT
## E        5       MUT
## F        6       MUT
```

## Methods
A generic function, or generic for short, is a function that dispatches methods
to their appropriate class-specific implementation. A method `foo` will work in
a way `X` with an object of class `A` and in a different way `Y` with an object
of class `B`.

We define methods using `setMethod`. Before we do so, we need to check if such
a method already exists. If it does, then a generic also exists and we can
proceed normally with the new method.
If it does not, then we first need to create the generic and
then proceed with the new method.

### `show` method
The `show` method allows to define a convenient and more compact view of an
object when we type its name in the console.


```r
show
```

```
## standardGeneric for "show" defined from package "methods"
## 
## function (object) 
## standardGeneric("show")
## <bytecode: 0x7fce2bbac460>
## <environment: 0x7fce2a32cf58>
## Methods may be defined for arguments: object
## Use  showMethods("show")  for currently available ones.
## (This generic function excludes non-simple inheritance; see ?setIs)
```

```r
isGeneric("show")
```

```
## [1] TRUE
```

```r
hasMethod("show")
```

```
## [1] TRUE
```

```r
showMethods("show")
```

```
## Function: show (package methods)
## object="ANY"
## object="classGeneratorFunction"
## object="classRepresentation"
## object="envRefClass"
## object="function"
##     (inherited from: object="ANY")
## object="genericFunction"
## object="genericFunctionWithTrace"
## object="MArray"
##     (inherited from: object="ANY")
## object="MethodDefinition"
## object="MethodDefinitionWithTrace"
## object="MethodSelectionReport"
## object="MethodWithNext"
## object="MethodWithNextWithTrace"
## object="namedList"
## object="ObjectsWithPackage"
## object="oldClass"
## object="refClassRepresentation"
## object="refMethodDef"
## object="refObjectGenerator"
## object="signature"
## object="sourceEnvironment"
## object="standardGeneric"
##     (inherited from: object="genericFunction")
## object="traceable"
```

```r
args("show")
```

```
## function (object) 
## NULL
```

As there is already a `show` method, we can proceed with the new method
definition using the `setMethod` function. We need a couple of things first:

* the class of the object we're implementing the method for:
    - MArray passed via the `signature` argument;
* the argument names that are defined in the generic:
    - use `args("show")` or just `show`;
    - this gives a single argument called `object`;
    - this name will need to be used when writing the `definition` of our
      method;


```r
ma
```

```
## An object of class "MArray"
## Slot "marray":
##                 A         B         C         D         E         F
## probe1  16.854792 16.524348  8.466807 12.277251 11.029993 11.609626
## probe2   7.176509 21.433227  1.093458 13.524187  8.194714  6.080805
## probe3  11.815642  3.055696  9.140413 15.175518 13.790816 17.878638
## probe4  13.164313  8.606056 16.073373  6.955368  6.366476 13.214497
## probe5  12.021342  9.333393 19.475967 12.524776  3.158595 10.448803
## probe6   9.469377 13.179752  7.847654  1.414957 12.164090 11.382754
## probe7  17.557610  8.578735  8.713653  6.077705  5.943034 13.396444
## probe8   9.526705 -3.282277  1.184185  5.745462 17.220506 10.449164
## probe9  20.092119 -2.202335 12.300487 -2.071038  7.842769 -4.965450
## probe10  9.686430 16.600567  6.800026 10.180613 13.278239 11.424415
## 
## Slot "samp_annot":
##   sampleID condition
## A        1        WT
## B        2        WT
## C        3        WT
## D        4       MUT
## E        5       MUT
## F        6       MUT
## 
## Slot "prob_annot":
##         geneID pathway
## probe1       1       J
## probe2       2       K
## probe3       3       O
## probe4       4       P
## probe5       5       S
## probe6       6       K
## probe7       7       X
## probe8       8       Z
## probe9       9       G
## probe10     10       S
```

```r
setMethod("show",
	  signature = "MArray",
	  definition = function(object) {
	      cat("An object of class ", class(object), "\n", sep = "")
	      cat(" ", nrow(object@marray), " features by ",
		  ncol(object@marray), " samples. \n", sep = "")
	      invisible(NULL)
	  })
```

```
## [1] "show"
```

```r
ma
```

```
## An object of class MArray
##  10 features by 6 samples.
```

### Accessor method
Let's create a method `marray` to access the `marray` slot from the `MArray`
class.

* First we create a new generic function with `setGeneric`. The name of the
  generic is defined, together with the name of the argument(s) that will have
  to be re-used when defining class-specific methods.


```r
setGeneric("marray", function(object, ...) standardGeneric("marray"))
```

```
## [1] "marray"
```

* Now we can create the new method using `setMethod`:


```r
setMethod("marray", "MArray",
	  function(object) object@marray)
```

```
## [1] "marray"
```

```r
marray(ma)
```

```
##                 A         B         C         D         E         F
## probe1  16.854792 16.524348  8.466807 12.277251 11.029993 11.609626
## probe2   7.176509 21.433227  1.093458 13.524187  8.194714  6.080805
## probe3  11.815642  3.055696  9.140413 15.175518 13.790816 17.878638
## probe4  13.164313  8.606056 16.073373  6.955368  6.366476 13.214497
## probe5  12.021342  9.333393 19.475967 12.524776  3.158595 10.448803
## probe6   9.469377 13.179752  7.847654  1.414957 12.164090 11.382754
## probe7  17.557610  8.578735  8.713653  6.077705  5.943034 13.396444
## probe8   9.526705 -3.282277  1.184185  5.745462 17.220506 10.449164
## probe9  20.092119 -2.202335 12.300487 -2.071038  7.842769 -4.965450
## probe10  9.686430 16.600567  6.800026 10.180613 13.278239 11.424415
```

* Let's do the same for the `samp_annot` and `prob_annot` slots:


```r
setGeneric("samp_annot", function(object, ...) standardGeneric("samp_annot"))
```

```
## [1] "samp_annot"
```

```r
setMethod("samp_annot", "MArray",
	  function(object) object@samp_annot)
```

```
## [1] "samp_annot"
```

```r
samp_annot(ma)
```

```
##   sampleID condition
## A        1        WT
## B        2        WT
## C        3        WT
## D        4       MUT
## E        5       MUT
## F        6       MUT
```

```r
setGeneric("prob_annot", function(object, ...) standardGeneric("prob_annot"))
```

```
## [1] "prob_annot"
```

```r
setMethod("prob_annot", "MArray",
	  function(object) object@prob_annot)
```

```
## [1] "prob_annot"
```

```r
prob_annot(ma)
```

```
##         geneID pathway
## probe1       1       J
## probe2       2       K
## probe3       3       O
## probe4       4       P
## probe5       5       S
## probe6       6       K
## probe7       7       X
## probe8       8       Z
## probe9       9       G
## probe10     10       S
```

### Subset method
We can create a subset method for an `MArray` object using the `[` operator.
Let's first look at a couple of usage examples:


```r
letters[1:5]
```

```
## [1] "a" "b" "c" "d" "e"
```

```r
`[`(letters, 1:5) # just like a function!
```

```
## [1] "a" "b" "c" "d" "e"
```

We can directly implement this subsetting operator for the `MArray` class since
primitive functions are automatically promoted to generics when a method of the
same name is defined.

Using `?[` we can see that the function takes four arguments: the `x` object,
the `i` and `j` indices and the `drop` logical. We can ignore the final
argument by using the "missing" value:


```r
setMethod("[", "MArray", 
	  function(x, i, j, drop = "missing") {
	      .marray <- x@marray[i, j]
	      .prob_annot <- x@prob_annot[i, ]
	      .samp_annot <- x@samp_annot[j, ]
	      MArray(marray = .marray,
		     prob_annot = .prob_annot,
		     samp_annot = .samp_annot)
	  })
```

```
## [1] "["
```

```r
ma[1:3, c(2, 5)] # first 3 probes of samples 2 and 5
```

```
## An object of class MArray
##  3 features by 2 samples.
```

### Validity method
As discussed earlier, the number of rows in the `marray` slot needs to be equal
to the number of rows in the `prob_annot` slot, while the number of columns
needs to be equal to the number of rows in the `samp_annot` slot. The
respective names also need to be identical:


```r
setValidity("MArray", function(object) {
		msg <- NULL
		valid <- TRUE
		if (nrow(marray(object)) != nrow(prob_annot(object))) {
		    valid <- FALSE
		    msg <- c(msg, paste0("Number of data and probe annotation",
				    " rows must be identical."))
		}
		if (ncol(marray(object)) != nrow(samp_annot(object))) {
		    valid <- FALSE
		    msg <- c(msg, paste0("Number of data columns and sample ",
					 "annotation rows must be identical."))
		}
		if (!identical(rownames(marray(object)),
			       rownames(prob_annot(object)))) {
		    valid <- FALSE
		    msg <- c(msg, paste0("Data and probe annotation row names",
					 " must be identical"))
		}
		if (!identical(colnames(marray(object)),
			       rownames(samp_annot(object)))) {
		    valid <- FALSE
		    msg <- c(msg, paste0("Data column names and ",
					 "sample annotation row names",
					 " must be identical"))
		}
		if (valid) TRUE else msg
	  })
```

```
## Class "MArray" [in ".GlobalEnv"]
## 
## Slots:
##                                        
## Name:      marray samp_annot prob_annot
## Class:     matrix data.frame data.frame
```

```r
validObject(ma)
```

```
## [1] TRUE
```
How about creating an invalid object?


```r
MArray(marray = matrix(1:12, ncol = 3),
       prob_annot = prob_annot(ma),
       samp_annot = samp_annot(ma))
```

```
## Error in validObject(.Object): invalid class "MArray" object: 1: Number of data and probe annotation rows must be identical.
## invalid class "MArray" object: 2: Number of data columns and sample annotation rows must be identical.
## invalid class "MArray" object: 3: Data and probe annotation row names must be identical
## invalid class "MArray" object: 4: Data column names and sample annotation row names must be identical
```

### Replacement method
How can we update the content of slots? We can do so directly, but this
violates the encapsulation of our data and makes it possible to break the
validity of an object.


```r
ma@marray <- 1
```

```
## Error in checkAtAssignment(structure("MArray", package = ".GlobalEnv"), : assignment of an object of class "numeric" is not valid for @'marray' in an object of class "MArray"; is(value, "matrix") is not TRUE
```

```r
(broken <- ma)
```

```
## An object of class MArray
##  10 features by 6 samples.
```

```r
broken@marray <- matrix(1:9, ncol = 3)
broken
```

```
## An object of class MArray
##  3 features by 3 samples.
```

```r
validObject(broken)
```

```
## Error in validObject(broken): invalid class "MArray" object: 1: Number of data and probe annotation rows must be identical.
## invalid class "MArray" object: 2: Number of data columns and sample annotation rows must be identical.
## invalid class "MArray" object: 3: Data and probe annotation row names must be identical
## invalid class "MArray" object: 4: Data column names and sample annotation row names must be identical
```

We can use a special type of replacement method using the code
`slot(object)<-syntax`. You need to concatenate the name of the method and the
assignment operator. A method to replace the slot that can be accessed with the
`marray` accessor will be called `marray<-`. It requires at least two
arguments: the `object` to be updated and the replacement `value`.
And since there is no `marray<-` generic, we first need to define one:


```r
setGeneric("marray<-",
	   function(object, value) standardGeneric("marray<-"))
```

```
## [1] "marray<-"
```

In the definition of the replacement method we need to check that the
user-specified `value` does not break the validity of `object`:


```r
setMethod("marray<-", "MArray",
	  function(object, value) {
	      object@marray <- value
	      if (validObject(object))
		  return(object)
	  })
```

```
## [1] "marray<-"
```

Now we can try it out:


```r
tmp <- matrix(rnorm(n * m, 10, 5), ncol = m)
marray(ma) <- tmp
```

```
## Error in validObject(object): invalid class "MArray" object: 1: Data and probe annotation row names must be identical
## invalid class "MArray" object: 2: Data column names and sample annotation row names must be identical
```

```r
colnames(tmp) <- LETTERS[1:m]
rownames(tmp) <- paste0("probe", 1:n)
head(marray(ma), n = 2) # before
```

```
##                A        B        C        D         E         F
## probe1 16.854792 16.52435 8.466807 12.27725 11.029993 11.609626
## probe2  7.176509 21.43323 1.093458 13.52419  8.194714  6.080805
```

```r
marray(ma) <- tmp
head(marray(ma), n = 2) # after
```

```
##               A        B         C        D         E        F
## probe1 16.51271 12.90498 13.059984 5.696037 10.527569 8.088331
## probe2 11.67924 13.84089  8.914301 4.341307  7.888721 7.436749
```

* Let's do the same for the `samp_annot` and `prob_annot` slots:


```r
# First the generics
setGeneric("prob_annot<-",
	   function(object, value) standardGeneric("prob_annot<-"))
```

```
## [1] "prob_annot<-"
```

```r
setGeneric("samp_annot<-",
	   function(object, value) standardGeneric("samp_annot<-"))
```

```
## [1] "samp_annot<-"
```

```r
# Now the methods
setMethod("prob_annot<-", "MArray",
	  function(object, value) {
	      object@prob_annot <- value
	      if (validObject(object))
		  return(object)
	  })
```

```
## [1] "prob_annot<-"
```

```r
setMethod("samp_annot<-", "MArray",
	  function(object, value) {
	      object@samp_annot <- value
	      if (validObject(object))
		  return(object)
	  })
```

```
## [1] "samp_annot<-"
```

Now we can try them out:


```r
samp_annot(ma)$sex <- rep(c("M", "F"), 3)
samp_annot(ma)
```

```
##   sampleID condition sex
## A        1        WT   M
## B        2        WT   F
## C        3        WT   M
## D        4       MUT   F
## E        5       MUT   M
## F        6       MUT   F
```

### Dissection methods
We can find out more about a class without reading its source code by using the
following functions:


```r
slotNames(ma) 
```

```
## [1] "marray"     "samp_annot" "prob_annot"
```

```r
getClass("MArray") # shows slots and their classes
```

```
## Class "MArray" [in ".GlobalEnv"]
## 
## Slots:
##                                        
## Name:      marray samp_annot prob_annot
## Class:     matrix data.frame data.frame
```

```r
showMethods("marray")
```

```
## Function: marray (package .GlobalEnv)
## object="MArray"
```

```r
showMethods(classes = "MArray")
```

```
## Function: [ (package base)
## x="MArray"
## 
## Function: initialize (package methods)
## .Object="MArray"
##     (inherited from: .Object="ANY")
## 
## Function: marray (package .GlobalEnv)
## object="MArray"
## 
## Function: marray<- (package .GlobalEnv)
## object="MArray"
## 
## Function: prob_annot (package .GlobalEnv)
## object="MArray"
## 
## Function: prob_annot<- (package .GlobalEnv)
## object="MArray"
## 
## Function: samp_annot (package .GlobalEnv)
## object="MArray"
## 
## Function: samp_annot<- (package .GlobalEnv)
## object="MArray"
## 
## Function: show (package methods)
## object="MArray"
```

```r
getMethod("marray", "MArray") # show method code
```

```
## Method Definition:
## 
## function (object, ...) 
## {
##     .local <- function (object) 
##     object@marray
##     .local(object, ...)
## }
## 
## Signatures:
##         object  
## target  "MArray"
## defined "MArray"
```

```r
getMethod("prob_annot", "MArray") 
```

```
## Method Definition:
## 
## function (object, ...) 
## {
##     .local <- function (object) 
##     object@prob_annot
##     .local(object, ...)
## }
## 
## Signatures:
##         object  
## target  "MArray"
## defined "MArray"
```


### Sources:
* https://github.com/lgatto/S4-tutorial
