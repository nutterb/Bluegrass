[![Build Status](https://travis-ci.org/nutterb/Bluegrass.png?branch=master)](https://travis-ci.org/nutterb/Bluegrass)

# Bluegrass

Bluegrass is a collection of tools I have developed for use in performing my professional responsibilities.  There is no central purpose to these functions, and the package does
not attempt to focus on any one task.


### Branch Structure

Please be aware that I may add, remove, or modify any function in Bluegrass at any time.  

* Added functions are unlikely to cause problems with existing code.  
* Modified functions may produce problems with back compatibility.  Modifications may be made when I feel like default options are no longer desired, or the order of function arguments should be changed, or arguments should be renamed.
* Functions that are removed will typically be moved to another repository of mine named `junkyard`.  

In general, I will use the following structure of branches:

#### Master
The most stable functions will stay in the **master** branch.  These are functions I believe work well, I use often, and are unlikely to be significantly changed.  A list of stable functions will be maintained below.

#### devel
The **devel** branch will be the primary source for updates and experimental code.  This branch will generally have more features thatn **master**, but these features may change or disappear suddenly.

Established features will be 

#### devel-[feature]
Development of new features will occur primarily in a devel branch named for the feature.  These branches may not have all of the features available in **devel** depending on when new features are considered stable enough to merge into **devel**.

### Currently Stable Functions

* firstRecord
* gestAge
* ggSurvGraph
* labelsTemplate
* lastRecord
* nthRecord
* percentileWtGestAge
* showDuplicated
* vectorCode
