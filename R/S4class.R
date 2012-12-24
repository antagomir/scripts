#S4 class creation example 
# Taken from http://cran.r-project.org/doc/contrib/Genolini-S4tutorialV0-5en.pdf
# IV. Memo. C.1.

# Class Creation
setClass (
          Class = "NewClass",
          representation = representation(x = "numeric", y = "character"),
          prototype = prototype (x = 1, y = "A"),
          contains = c("FatherClass"),
          validity = function (object) {return(TRUE)}
          )

# Object creation
new(Class = "NewClass")
A <- new(Class = "NewClass", x=2, y="B")

# Slot manipulation
A@x <- 4

# Class destruction (partial)
removeClass("NewClass")

# Constructor
newClass <- function () {
  ....
  return(new(Class="NewClass"))
}

# Validation
setMethod(f="initialize", signature = "NewClass",
          definition = function (.Object, value){
            if(....){stop("initialize (NewClass): Error")}else{}
            .Object@x <- value;
            validObject(.Object)
            return(.Object)
          }
)


### Accessor
# Getter
setGeneric(name="getX", def=function(object){standardGeneric("getX")})
setMethod(f="getX", signature = "NewClass",
          definition = function (object) {return(object@x)}
)

# Setter
setGeneric(name="setX<-", def=function(object,value){standardGeneric("setX<-")})
setReplaceMethod(f="setX", signature = "NewClass",
          def = function (object,value) {object@x <- value; return(object)}
)

### Methods

# To create a generic method
setGeneric(f = "newFunction", def=function(z,r){standardGeneric("newFunction")})
# To declare a method
setMethod(f="newFunction", signature="NewClass",
          def = function(z,r) {....; return(....)}
)

# To get the arguments of a function
args(NewFunction)

