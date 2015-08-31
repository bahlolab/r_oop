S4 Object-Oriented Programming
==============================
Here I'll keep notes about OOP using the S4 object system.

Classes and Instances/Objects
-----------------------------
A class is created using `setClass`. This takes the following key arguments:

* A __name__ for the class;
* A named list of __slots__ for the class;
* A class that it __contains__ i.e. inherits from.
* A default __prototype__ for the class;



```r
setClass("Person",
	 slots = list(
	   name = "character", age = "numeric"),
	 prototype = list(
	   name = NA_character_, age = NA_real_)
	 )

# Employee inherits Person
setClass("Employee",
	 slots = list(boss = "Person"),
	 contains = "Person")
getSlots("Person")
```

```
##        name         age 
## "character"   "numeric"
```

```r
getSlots("Employee")
```

```
##        boss        name         age 
##    "Person" "character"   "numeric"
```

Create a new object of a class:


```r
nat <- new("Person", name = "Natalie", age = 36)
peter <- new("Employee", name = "Peter", age = 30, boss = nat)
nat
```

```
## An object of class "Person"
## Slot "name":
## [1] "Natalie"
## 
## Slot "age":
## [1] 36
```

```r
peter
```

```
## An object of class "Employee"
## Slot "boss":
## An object of class "Person"
## Slot "name":
## [1] "Natalie"
## 
## Slot "age":
## [1] 36
## 
## 
## Slot "name":
## [1] "Peter"
## 
## Slot "age":
## [1] 30
```

Generic functions and methods
------------------------------
In order to define a method for a specific class, you need to make sure that a
generic method is defined for your method.
But how can you examine if a method has a generic?

* All primitive functions (e.g. check `getGenerics()`) have implicit generics;
* Use `isGeneric()`;
* Use `show()`;


```r
getGenerics()
```

```
## An object of class "ObjectsWithPackage":
##                                                                        
## Object:  "-"    "!="   "["    "[[<-" "[<-"  "*"    "/"    "&"    "%/%" 
## Package: "base" "base" "base" "base" "base" "base" "base" "base" "base"
##                                                                        
## Object:  "%%"   "^"    "+"    "<"    "<="   "=="   ">"    ">="   "|"   
## Package: "base" "base" "base" "base" "base" "base" "base" "base" "base"
##                                                                     
## Object:  "$"    "$<-"  "abs"  "acos" "acosh" "addNextMethod" "Arith"
## Package: "base" "base" "base" "base" "base"  "methods"       "base" 
##                                                                    
## Object:  "asin" "asinh" "atan" "atanh" "body<-" "cbind2"  "ceiling"
## Package: "base" "base"  "base" "base"  "base"   "methods" "base"   
##                                                                        
## Object:  "coerce"  "coerce<-" "Compare" "Complex" "cos"  "cosh" "cospi"
## Package: "methods" "methods"  "methods" "base"    "base" "base" "base" 
##                                                                       
## Object:  "cummax" "cummin" "cumprod" "cumsum" "digamma" "exp"  "expm1"
## Package: "base"   "base"   "base"    "base"   "base"    "base" "base" 
##                                                                        
## Object:  "floor" "gamma" "initialize" "kronecker" "lgamma" "loadMethod"
## Package: "base"  "base"  "methods"    "base"      "base"   "methods"   
##                                                                       
## Object:  "log"  "log10" "log1p" "log2" "Logic" "Math" "Math2"   "Ops" 
## Package: "base" "base"  "base"  "base" "base"  "base" "methods" "base"
##                                                                           
## Object:  "rbind2"  "round" "show"    "sign" "signif" "sin"  "sinh" "sinpi"
## Package: "methods" "base"  "methods" "base" "base"   "base" "base" "base" 
##                                                                         
## Object:  "slotsFromS3" "sqrt" "Summary" "tan"  "tanh" "tanpi" "trigamma"
## Package: "methods"     "base" "base"    "base" "base" "base"  "base"    
##                 
## Object:  "trunc"
## Package: "base"
```

```r
isGeneric("age")
```

```
## [1] FALSE
```

```r
show(age)
```

```
## Error in show(age): error in evaluating the argument 'object' in selecting a method for function 'show': Error: object 'age' not found
```

```r
isGeneric("name")
```

```
## [1] FALSE
```

```r
show(name)
```

```
## Error in show(name): error in evaluating the argument 'object' in selecting a method for function 'show': Error: object 'name' not found
```


```r
setGeneric("age", function(obj, ...) standardGeneric("age"))
```

```
## [1] "age"
```

```r
setMethod("age", "Person", function(obj, ...) obj@age)
```

```
## [1] "age"
```

```r
setGeneric("name", function(obj, ...) standardGeneric("name"))
```

```
## [1] "name"
```

```r
setMethod("name", "Person", function(obj, ...) obj@name)
```

```
## [1] "name"
```

```r
isGeneric("age")
```

```
## [1] TRUE
```

```r
show(age)
```

```
## standardGeneric for "age" defined from package ".GlobalEnv"
## 
## function (obj, ...) 
## standardGeneric("age")
## <environment: 0x7fa4c8122a28>
## Methods may be defined for arguments: obj
## Use  showMethods("age")  for currently available ones.
```

```r
isGeneric("name")
```

```
## [1] TRUE
```

```r
show(name)
```

```
## standardGeneric for "name" defined from package ".GlobalEnv"
## 
## function (obj, ...) 
## standardGeneric("name")
## <environment: 0x7fa4c59fe508>
## Methods may be defined for arguments: obj
## Use  showMethods("name")  for currently available ones.
```

```r
age(nat)
```

```
## [1] 36
```

```r
name(nat)
```

```
## [1] "Natalie"
```

```r
str(nat)
```

```
## Formal class 'Person' [package ".GlobalEnv"] with 2 slots
##   ..@ name: chr "Natalie"
##   ..@ age : num 36
```

Validity methods
----------------
You can define methods that apply additional restrictions for the validity of an object:


```r
check_person <- function(object) {
  errors <- character()
  length_age <- length(object@age)
  if (length_age != 1) {
    msg <- paste0("Age is length ", length_age, ". Should be 1.")
    errors <- c(errors, msg)
  }
  
  length_name <- length(object@name)
  if (length_name != 1) {
    msg <- paste0("Name is length ", length_name, ". Should be 1.")
    errors <- c(errors, msg)
    
  }
  
  if (length(errors) == 0) TRUE else errors
}
```

Let's try it out:


```r
setClass("Person",
         slots = list(name = "character",
                      age = "numeric"),
         validity = check_person)
new("Person", name = "Hadley")
```

```
## Error in validObject(.Object): invalid class "Person" object: Age is length 0. Should be 1.
```

```r
new("Person", name = "Hadley", age = 1:10)
```

```
## Error in validObject(.Object): invalid class "Person" object: Age is length 10. Should be 1.
```

But note that the check is not automatically applied when we modify slots directly:


```r
hadley <- new("Person", name = "Hadley", age = 31)
hadley@age <- 1:10
hadley
```

```
## An object of class "Person"
## Slot "name":
## [1] "Hadley"
## 
## Slot "age":
##  [1]  1  2  3  4  5  6  7  8  9 10
```

You can force a check with `validObject`:


```r
validObject(hadley)
```

```
## Error in validObject(hadley): invalid class "Person" object: Age is length 10. Should be 1.
```

Accessor methods
----------------
You can access a slot of an object using `@` or `slot`:


```r
peter@age
```

```
## [1] 30
```

```r
slot(peter, "age")
```

```
## [1] 30
```

```r
nat@age
```

```
## [1] 36
```

```r
slot(nat, "age")
```

```
## [1] 36
```

```r
age(nat) # not defined yet
```

```
## [1] 36
```

But this is not an efficient way of doing so. You should instead define
an accessor method (_getter_). 

Display Source Code
-------------------
Let's say I'm using the `QDNAseq` package, and I want to see what the `plot`
function does with objects of class `QDNAseqReadCounts`. I need to use the
`getMethod` function or the selectMethod if the class inherits from a parent
class:


```r
require(QDNAseq)
```

```
## Loading required package: QDNAseq
## Creating a generic function for 'nchar' from package 'base' in package 'S4Vectors'
```

```r
methods("plot")
```

```
##  [1] plot,ANY,ANY-method                plot,cghCall,missing-method       
##  [3] plot,cghRaw,missing-method         plot,cghSeg,missing-method        
##  [5] plot,profile.mle,missing-method    plot,QDNAseqSignals,missing-method
##  [7] plot.acf*                          plot.cghCall*                     
##  [9] plot.cghRaw*                       plot.cghRegions*                  
## [11] plot.cghSeg*                       plot.data.frame*                  
## [13] plot.decomposed.ts*                plot.default                      
## [15] plot.dendrogram*                   plot.density*                     
## [17] plot.DNAcopy*                      plot.ecdf                         
## [19] plot.factor*                       plot.formula*                     
## [21] plot.function                      plot.hclust*                      
## [23] plot.histogram*                    plot.HoltWinters*                 
## [25] plot.isoreg*                       plot.lm*                          
## [27] plot.marrayNorm*                   plot.marrayRaw*                   
## [29] plot.medpolish*                    plot.mlm*                         
## [31] plot.ppr*                          plot.prcomp*                      
## [33] plot.princomp*                     plot.profile.nls*                 
## [35] plot.raster*                       plot.spec*                        
## [37] plot.stepfun                       plot.stl*                         
## [39] plot.table*                        plot.ts                           
## [41] plot.tskernel*                     plot.TukeyHSD*                    
## see '?methods' for accessing help and source code
```

```r
showMethods("plot")
```

```
## Function: plot (package graphics)
## x="ANY", y="ANY"
## x="cghCall", y="missing"
## x="cghRaw", y="missing"
## x="cghSeg", y="missing"
## x="profile.mle", y="missing"
## x="QDNAseqSignals", y="missing"
```

```r
selectMethod("plot", signature = c("QDNAseqReadCounts", "missing"))
```

```
## Method Definition:
## 
## function (x, y, ...) 
## {
##     .local <- function (x, y, main = NULL, includeReadCounts = TRUE, 
##         logTransform = TRUE, scale = TRUE, sdFUN = "sdDiffTrim", 
##         delcol = "darkred", losscol = "red", gaincol = "blue", 
##         ampcol = "darkblue", pointcol = "black", segcol = "chocolate", 
##         misscol = NA, xlab = "chromosomes", ylab = NULL, ylim = NULL, 
##         xaxt = "s", yaxp = NULL, showDataPoints = TRUE, showSD = TRUE, 
##         pointpch = 1, pointcex = 0.1, doSegments = TRUE, doCalls = TRUE, 
##         ...) 
##     {
##         if (inherits(x, c("QDNAseqCopyNumbers", "QDNAseqReadCounts"))) {
##             condition <- binsToUse(x)
##         }
##         else {
##             condition <- rep(TRUE, times = nrow(x))
##         }
##         baseLine <- NA
##         doCalls <- "calls" %in% assayDataElementNames(x) & doCalls
##         doSegments <- "segmented" %in% assayDataElementNames(x) & 
##             doSegments
##         if (doCalls) {
##             if (is.null(ylim)) 
##                 if (logTransform) {
##                   ylim <- c(-5, 5)
##                 }
##                 else {
##                   ylim <- c(-2, 4)
##                 }
##         }
##         if ("copynumber" %in% assayDataElementNames(x)) {
##             copynumber <- assayDataElement(x, "copynumber")[condition, 
##                 , drop = FALSE]
##             if (is.null(ylab)) 
##                 ylab <- ifelse(logTransform, expression(log[2] ~ 
##                   ratio), "ratio")
##             if (is.null(ylim)) 
##                 if (logTransform) {
##                   ylim <- c(-3, 5)
##                 }
##                 else {
##                   ylim <- c(0, 4)
##                 }
##             if (is.null(yaxp)) 
##                 yaxp <- c(ylim[1], ylim[2], ylim[2] - ylim[1])
##             baseLine <- ifelse(logTransform, 0, 1)
##         }
##         else {
##             copynumber <- assayDataElement(x, "counts")[condition, 
##                 , drop = FALSE]
##             if (is.null(ylab)) 
##                 ylab <- ifelse(logTransform, expression(log[2] ~ 
##                   read ~ count), "read count")
##             if (is.null(ylim)) 
##                 if (logTransform) {
##                   ylim <- c(0, max(log2adhoc(copynumber)))
##                 }
##                 else {
##                   ylim <- range(copynumber)
##                 }
##         }
##         if (is.null(main)) 
##             main <- sampleNames(x)
##         if (includeReadCounts && "total.reads" %in% names(pData(x))) 
##             main <- paste(main, " (", format(x$total.reads, trim = TRUE, 
##                 big.mark = ","), " reads)", sep = "")
##         if (length(ylab) == 1) 
##             ylab <- rep(ylab, times = ncol(x))
##         all.chrom <- chromosomes(x)
##         chrom <- all.chrom[condition]
##         uni.chrom <- unique(chrom)
##         if (!scale) {
##             pos <- pos2 <- 1:sum(condition)
##             chrom.ends <- aggregate(pos, by = list(chromosome = chrom), 
##                 FUN = max)$x
##         }
##         else {
##             if (inherits(x, c("cghRaw", "cghSeg", "cghCall"))) {
##                 chrom.lengths <- CGHbase:::.getChromosomeLengths("GRCh37")
##             }
##             else {
##                 all.chrom.lengths <- aggregate(bpend(x), by = list(chromosome = all.chrom), 
##                   FUN = max)
##                 chrom.lengths <- all.chrom.lengths$x
##                 names(chrom.lengths) <- all.chrom.lengths$chromosome
##             }
##             pos <- as.numeric(bpstart(x)[condition])
##             pos2 <- as.numeric(bpend(x)[condition])
##             chrom.lengths <- chrom.lengths[as.character(uni.chrom)]
##             chrom.ends <- integer()
##             cumul <- 0
##             for (i in uni.chrom) {
##                 pos[chrom > i] <- pos[chrom > i] + chrom.lengths[as.character(i)]
##                 pos2[chrom > i] <- pos2[chrom > i] + chrom.lengths[as.character(i)]
##                 cumul <- cumul + chrom.lengths[as.character(i)]
##                 chrom.ends <- c(chrom.ends, cumul)
##             }
##             names(chrom.ends) <- names(chrom.lengths)
##         }
##         if (inherits(x, c("cghRaw", "cghSeg", "cghCall"))) 
##             copynumber <- log2adhoc(copynumber, inv = TRUE)
##         if (is.character(sdFUN) && sdFUN == "sdDiffTrim") {
##             symbol <- quote(hat(sigma)[Delta^"*"])
##         }
##         else if (is.character(sdFUN) && length(grep("Diff", sdFUN)) == 
##             1) {
##             symbol <- quote(hat(sigma)[Delta])
##         }
##         else {
##             symbol <- quote(hat(sigma))
##         }
##         sdFUN <- match.fun(sdFUN)
##         noise <- apply(copynumber, 2, sdFUN, na.rm = TRUE)
##         if (logTransform) 
##             copynumber <- log2adhoc(copynumber)
##         for (i in seq_len(ncol(x))) {
##             vmsg("Plotting sample ", main[i], " (", i, " of ", 
##                 ncol(x), ") ...", appendLF = FALSE)
##             cn <- copynumber[, i]
##             if (doSegments) {
##                 segmented <- assayDataElement(x, "segmented")[condition, 
##                   i]
##                 if (inherits(x, c("cghRaw", "cghSeg", "cghCall"))) 
##                   segmented <- log2adhoc(segmented, inv = TRUE)
##                 if (logTransform) 
##                   segmented <- log2adhoc(segmented)
##                 segment <- CGHbase:::.makeSegments(segmented, 
##                   chrom)
##             }
##             if (doCalls) {
##                 losses <- probloss(x)[condition, i]
##                 gains <- probgain(x)[condition, i]
##                 if (!is.null(probdloss(x))) 
##                   losses <- losses + probdloss(x)[condition, 
##                     i]
##                 if (!is.null(probamp(x))) 
##                   gains <- gains + probamp(x)[condition, i]
##                 par(mar = c(5, 4, 4, 4) + 0.2)
##                 plot(NA, main = main[i], xlab = NA, ylab = NA, 
##                   las = 1, xlim = c(0, max(pos2)), ylim = ylim, 
##                   xaxs = "i", xaxt = "n", yaxp = c(ylim[1], ylim[2], 
##                     ylim[2] - ylim[1]), yaxs = "i")
##                 lim <- par("usr")
##                 lim[3:4] <- c(0, 1)
##                 par(usr = lim)
##                 dticks <- seq(0, 1, by = 0.2)
##                 axis(4, at = dticks, labels = NA, srt = 270, 
##                   las = 1, cex.axis = 1, cex.lab = 1, tck = -0.015)
##                 axis(4, at = dticks, labels = dticks, srt = 270, 
##                   las = 1, cex.axis = 1, cex.lab = 1, line = -0.4, 
##                   lwd = 0)
##                 mtext("probability", side = 4, line = 2, cex = par("cex"))
##                 if (!is.na(misscol)) {
##                   rect(0, -1, max(pos2), 1, col = misscol, border = NA)
##                   rect(pos, -1, pos2, 1, col = "white", border = NA)
##                 }
##                 rect(pos[segment[, 2]], 0, pos2[segment[, 3]], 
##                   losses[segment[, 2]], col = losscol, border = losscol)
##                 rect(pos[segment[, 2]], 1, pos2[segment[, 3]], 
##                   1 - gains[segment[, 2]], col = gaincol, border = gaincol)
##                 axis(3, at = pos[which(probamp(x)[condition, 
##                   i] >= 0.5)], labels = FALSE, col = ampcol, 
##                   col.axis = "black", srt = 270, las = 1, cex.axis = 1, 
##                   cex.lab = 1)
##                 axis(1, at = pos[which(probdloss(x)[condition, 
##                   i] >= 0.5)], labels = FALSE, col = delcol, 
##                   col.axis = "black", srt = 270, las = 1, cex.axis = 1, 
##                   cex.lab = 1)
##                 box()
##                 lim[3:4] <- ylim
##                 par(usr = lim)
##                 points(pos, cn, cex = pointcex, col = pointcol, 
##                   pch = pointpch)
##             }
##             else {
##                 plot(pos, cn, cex = pointcex, col = pointcol, 
##                   main = main[i], xlab = NA, ylab = NA, ylim = ylim, 
##                   xaxt = "n", xaxs = "i", yaxs = "i", yaxp = yaxp, 
##                   tck = -0.015, las = 1, pch = pointpch)
##             }
##             mtext(text = xlab, side = 1, line = 2, cex = par("cex"))
##             mtext(text = ylab[i], side = 2, line = 2, cex = par("cex"))
##             abline(h = baseLine)
##             abline(v = chrom.ends[-length(chrom.ends)], lty = "dashed")
##             if (!is.na(xaxt) && xaxt != "n") {
##                 ax <- (chrom.ends + c(0, chrom.ends[-length(chrom.ends)]))/2
##                 axis(side = 1, at = ax, labels = NA, cex = 0.2, 
##                   lwd = 0.5, las = 1, cex.axis = 1, cex.lab = 1, 
##                   tck = -0.015)
##                 axis(side = 1, at = ax, labels = uni.chrom, cex = 0.2, 
##                   lwd = 0, las = 1, cex.axis = 1, cex.lab = 1, 
##                   tck = -0.015, line = -0.4)
##             }
##             if (doSegments) {
##                 for (jjj in seq_len(nrow(segment))) {
##                   segments(pos[segment[jjj, 2]], segment[jjj, 
##                     1], pos[segment[jjj, 3]], segment[jjj, 1], 
##                     col = segcol, lwd = 3)
##                 }
##             }
##             par(xpd = TRUE)
##             amps <- cn
##             amps[amps <= ylim[2]] <- NA_real_
##             amps[!is.na(amps)] <- ylim[2] + 0.01 * (ylim[2] - 
##                 ylim[1])
##             dels <- cn
##             dels[dels >= ylim[1]] <- NA_real_
##             dels[!is.na(dels)] <- ylim[1] - 0.01 * (ylim[2] - 
##                 ylim[1])
##             points(pos, amps, pch = 24, col = pointcol, bg = pointcol, 
##                 cex = 0.5)
##             points(pos, dels, pch = 25, col = pointcol, bg = pointcol, 
##                 cex = 0.5)
##             if (doSegments) {
##                 amps <- assayDataElement(x, "segmented")[condition, 
##                   i]
##                 amps[amps <= ylim[2]] <- NA_real_
##                 amps[!is.na(amps)] <- ylim[2] + 0.01 * (ylim[2] - 
##                   ylim[1])
##                 dels <- assayDataElement(x, "segmented")[condition, 
##                   i]
##                 dels[dels >= ylim[1]] <- NA_real_
##                 dels[!is.na(dels)] <- ylim[1] - 0.01 * (ylim[2] - 
##                   ylim[1])
##                 points(pos, amps, pch = 24, col = segcol, bg = segcol, 
##                   cex = 0.5)
##                 points(pos, dels, pch = 25, col = segcol, bg = segcol, 
##                   cex = 0.5)
##             }
##             par(xpd = FALSE)
##             if (showSD) {
##                 if (is.numeric(x$expected.variance[i])) {
##                   sdexp <- substitute(paste(E ~ sigma == e, ", ", 
##                     symbol == sd), list(e = sprintf("%.3g", sqrt(x$expected.variance[i])), 
##                     symbol = symbol, sd = sprintf("%.3g", noise[i])))
##                 }
##                 else {
##                   sdexp <- substitute(symbol == sd, list(symbol = symbol, 
##                     sd = sprintf("%.3g", noise[i])))
##                 }
##                 mtext(sdexp, side = 3, line = 0, adj = 1, cex = par("cex"))
##             }
##             if (showDataPoints) {
##                 str <- paste(round(sum(condition)/1000), "k x ", 
##                   sep = "")
##                 probe <- median(bpend(x) - bpstart(x) + 1)
##                 if (probe < 1000) {
##                   str <- paste(str, probe, " bp", sep = "")
##                 }
##                 else {
##                   str <- paste(str, round(probe/1000), " kbp", 
##                     sep = "")
##                 }
##                 if (doSegments) 
##                   str <- paste(str, ", ", nrow(segment), " segments", 
##                     sep = "")
##                 mtext(str, side = 3, line = 0, adj = 0, cex = par("cex"))
##             }
##             vmsg()
##         }
##         options(`QDNAseq::plotLogTransform` = logTransform)
##         options(`QDNAseq::plotScale` = scale)
##     }
##     .local(x, y, ...)
## }
## <environment: namespace:QDNAseq>
## 
## Signatures:
##         x                   y        
## target  "QDNAseqReadCounts" "missing"
## defined "QDNAseqSignals"    "missing"
```
