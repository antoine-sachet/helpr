#' ########################################
#' # Misc functions to manage data in rda files.
#' #
#' # Credit to:
#' # - https://faculty.washington.edu/jmiyamot/p548/rnote.pdf
#' #   https://faculty.washington.edu/jmiyamot/zmisc/downloads.htm
#' ########################################
#'
#' #' Move objects to and from .rda files
#' #' @seealso rm.sv
#' #' @export
#' move <- function( listObjects, to,
#'                   from             = ".GlobalEnv",
#'                   fn.object        = FALSE,
#'                   replace.objects  = FALSE,
#'                   move.doc.objects = TRUE,
#'                   copy.only        = FALSE,
#'                   feedback         = TRUE,
#'                   map              = TRUE  )  {
#'   #-------------------------------------------------------------
#'   # 'actionName' is set to "move" if the action is a move, and to "copy" if the action is a copy.
#'   # The following code sets the values of labels for moves or copies - these variables are
#'   # used later in creating error messages that reference a move or a copy.
#'   actionMatrix = matrix( c(
#'     "move", "copy",
#'     "moved", "copied"
#'   ), ncol = 2, byrow = TRUE)
#'
#'   dimnames(actionMatrix) = list(
#'     rows = c( "function", "pastParticiple"),
#'     columns = c( "move", "copy" )  )
#'
#'   if ( !copy.only ) actionName = actionMatrix[ , "move" ]
#'   if (  copy.only ) actionName = actionMatrix[ , "copy" ]
#'   names( actionName ) = rownames( actionMatrix )
#'   #-------------------------------------------------------------
#'   # Convert 'to' to 'env.on.path'.
#'   # Note 'env.on.path' (character) and 'dest.pos' (numeric) are alternative ways to specify the destination of the move.
#'   if (is.character(to)) {
#'     env.on.path = to
#'     dest.pos = NA
#'   } else {  #end 'if (is.character(to))'
#'     env.on.path = ""
#'     dest.pos = to
#'   } #end 'else' for 'if (is.character(to))'
#'
#'   # Warning & stop when the input list appears to be a character string.
#'   if (is.character(listObjects) &&
#'       any( listObjects != make.names(listObjects) ) ) {
#'     msg <- paste(
#'       "\n\nThe 'listObjects' argument appears to be a character string rather\n",
#'       "than a variable name or vector of variable names.  If 'listObjects' was\n",
#'       "set to a single string variable, put the name of the variable in\n",
#'       "quotes and reapply the '", actionName[ "function" ], "' function.\n\n",
#'       "Warning: No '", actionName[ "function" ],
#'       "' was made because it was not clear what variable\n",
#'       "to ", actionName[ "function" ],
#'       sep = "")
#'     stop(msg)
#'   } #endIf
#'
#'   # Convert 'env.on.path' to numeric position (env.pos) if 'env.on.path' is non-null.
#'   env.pos <- NA
#'
#'   if (env.on.path != "") {
#'
#'     env.pos <- grep(env.on.path, search(), ignore.case=TRUE)
#'
#'     # Stop if 'env.on.path' string is not contained within a name on the search path.
#'     if (length(env.pos) == 0) {
#'       msg <- paste(
#'         "The string specified by 'to' was not found within the search path names.\n",
#'         "No '",
#'         actionName[ "function" ],
#'         "' was carried out!",
#'         sep = "" )
#'       cat("search()\n")
#'       print(search())
#'       cat("\n")
#'       stop(msg) }
#'
#'     # Stop if 'env.on.path' string is contained within more than one name on the search path.
#'     if (length(env.pos) > 1) {
#'       msg <- paste(
#'         "More than one match was found for the 'to' argument among",
#'         "\nthe search path names.  No '",
#'         actionName[ "function" ],
#'         "' was carried out!",
#'         sep = "" )
#'       print(search())
#'       cat("\n")
#'       stop(msg) }
#'   } #end 'if (env.on.path != "")'
#'
#'   # Assign the correct number to env.pos = dest.pos, if it is not already assigned.
#'   if (is.na(dest.pos)) dest.pos <- env.pos
#'
#'   #-------------------------------------------------------------
#'
#'   # Convert 'from' to numeric position (src.pos) if 'env.on.path' is non-null.
#'   if (is.numeric(from)) src.pos = from
#'
#'   if (!is.numeric(from)) {
#'
#'     src.pos <- grep(from, search(), ignore.case=TRUE)
#'
#'     #cat("Pos 3:  inside 'if (!is.numeric(from))' \n",
#'     #"from = ", from, "\n",
#'     #"src.pos = ", src.pos, "\n")
#'
#'     # Stop if 'from' string is not contained within a name on the search path, and we are not moving
#'     # an object that is in the environment of a calling function.
#'     if ( length(src.pos) == 0 & !fn.object ) {
#'       msg <- paste(
#'         "The string specified by 'from' not found within search path names.\n",
#'         "No '",
#'         actionName[ "function" ],
#'         "' was carried out!",
#'         sep = "" )
#'       cat("search()\n")
#'       print(search())
#'       cat("\n")
#'       stop(msg) }
#'
#'     # Stop if 'from' string is contained within more than one name on the search path, and we are not moving
#'     # an object that is in the environment of a calling function.
#'     if ( length(src.pos) > 1 & !fn.object ) {
#'       msg <- paste(
#'         "More than one match found for 'from' argument among",
#'         "\nthe search path names.  No '",
#'         actionName[ "function" ],
#'         "' was carried out!",
#'         sep = "" )
#'       print(search())
#'       cat("\n")
#'       stop(msg) }
#'
#'   } #end 'if (!is.numeric(from))'
#'
#'   # #-------------------------------------------------------------
#'
#'   # If listObjects is not a character string, then extract its name.
#'   if (mode(listObjects) != "character")
#'     V.names <- deparse(substitute(listObjects)) else V.names <- listObjects
#'
#'   # If move.doc.objects = TRUE, then any listObjects.doc corresponding to listObjects will also be moved.
#'   if (move.doc.objects) {
#'     tm.names <- NULL
#'     for (i in 1:length(V.names)) {
#'
#'       tm.names <- c(tm.names, V.names[i])
#'
#'       doc.name.i = paste(V.names[i], '.doc', sep='')
#'       exists.i   = exists(doc.name.i, where = src.pos, inherits = FALSE)
#'       in.V.names = ( doc.name.i %in% V.names )
#'
#'       if ( exists.i & !in.V.names ) {
#'         tm.names <- c(tm.names, paste(V.names[i], '.doc', sep=''))
#'       } #end 'if (exists(paste(V.names[i], '.doc', sep='')))'
#'     } #end 'for (i in 1:length(V.names)'
#'     V.names <- tm.names
#'   } #end 'if (move.doc.objects)'
#'
#'   # Assign the correct file name to the file that receives the object(s).
#'   tm.target <- search()[dest.pos]
#'   # Stop the function if the destination is not a file; otherwise, extract its name.
#'   char.1.5 = substr(tm.target, 1, 5)
#'   if (char.1.5 != ".Glob" & char.1.5 != "file:") stop("\n",
#'                                                       "The environment to which objects are to be ",
#'                                                       actionName[ "pastParticiple" ],
#'                                                       " must be either\n",
#'                                                       "'.GlobalEnv' or a file that is designated 'file:....'.  Check that\n",
#'                                                       "the 'to' argument has been correctly specified.\n" ) else    #end stop
#'
#'                                                         target.name <- substr(tm.target, 6, nchar(tm.target))
#'   #end of 'else'
#'
#'   # e.source is a boolean vector.  e.source[i] == TRUE iff V.names[i] exists in pos = src.pos.
#'   if (fn.object) {
#'
#'     calling.fn.env = parent.frame()
#'
#'     e.source <- sapply(V.names,
#'                        function(x) exists(x, envir = calling.fn.env, inherits=F))
#'
#'   } else {  #endIf 'if (fn.object)'
#'     e.source <- sapply(V.names,
#'                        function(x) exists(x, where = src.pos, inherits=F))
#'   } #endElse 'if (fn.object)'
#'
#'   # e.target is a boolean vector.  e.target[i] == TRUE iff V.names[i] exists in pos = dest.pos.
#'   e.target <- sapply(V.names,
#'                      function(x) exists(x, where = dest.pos, inherits=F))
#'
#'   permit.move <- NULL
#'   # The next 'if' carries out the move, if this is possible.
#'   for (i in 1:length(V.names)) {
#'     permit.move <- c(permit.move,
#'                      (e.source[i] && (replace.objects | !e.target[i])))
#'
#'     if (permit.move[i]) {
#'       #cat("Pos 4:  i =", i, ":  Inside of permit.move[i]\n")
#'       if (fn.object) {
#'         #cat("Pos 5:  i = ", i, ":  V.names[i] = ", V.names[i], "\n", sep = "")
#'
#'         assign(V.names[i], get(V.names[i],
#'                                envir = calling.fn.env, inherits = FALSE), pos = dest.pos)
#'       } else {  #endIf 'if (fn.object)'
#'         #cat("Pos 6:  i = ", i, ":  V.names[i] = ", V.names[i], "\n", sep = "")
#'         assign(V.names[i], get(V.names[i], pos = src.pos), pos = dest.pos)
#'       } #endElse 'if (fn.object)'
#'
#'     } # end of 'if (permit.move[i])
#'
#'   } #end 'for (i in 1:length(V.names))'
#'
#'   # Save the objects to the target file after they have been moved, except if the objects have been
#'   # moved to .GlobalEnv
#'   if (dest.pos != 1) save(list = objects(pos = dest.pos, all.names = T),
#'                           file = target.name )
#'   if (!copy.only & any(permit.move))
#'     rm.sv( list = V.names[permit.move], pos = src.pos,
#'            rm.doc.objects = FALSE, feedback = FALSE)
#'   # Note that rm.doc.objects == FALSE because if move.doc.objects == TRUE, then the doc
#'   # objects are already listed in V.names so it is unnecessary to ask rm.sv to delete the doc
#'   # objects.  If move.doc.objects == FALSE, then V.names will not contain the doc object
#'   # names (except if they are explicitly specified) so the doc objects will not be deleted.
#'
#'   # The next code gives feedback about the objects that were moved.
#'   if (any(permit.move)) {
#'
#'     msg <- paste(
#'       "The following objects were ",
#'       actionName[ "pastParticiple" ],
#'       " from position ",
#'       src.pos, " (", search()[src.pos], ")\nto position ",
#'       dest.pos, " (", search()[dest.pos], "):\n", sep="")
#'
#'     if (feedback) { cat(msg); print(V.names[permit.move]) }
#'
#'   } else {
#'
#'     msg <- paste(
#'       "WARNING:  At least some of the objects were not ",
#'       actionName[ "pastParticiple" ],
#'       "\nfrom position ", src.pos, " (", search()[src.pos],
#'       ")\nto   position ",
#'       dest.pos, " (", search()[dest.pos], ").\n",
#'       sep="")
#'
#'     if (feedback) cat(msg)
#'   } #'else' clause of 'if (!any(permit.move))'
#'
#'   # The next code gives warning messages for objects that could not be moved.
#'   if (!all(e.source) & feedback) {
#'     cat(paste("\n",
#'               "The following objects could not be found in position ",
#'               src.pos, " (", search()[src.pos], "):\n\n", sep = ""))
#'     cat(paste( V.names[!e.source], collapse = ", "), "\n")
#'   } #end 'if (all(e.source) != TRUE)'
#'
#'   if ((!replace.objects) & any(e.source & e.target) & feedback) {
#'     msg1 <- paste("\n",
#'                   "The following objects already exist in position ", dest.pos,
#'                   " (", search()[dest.pos], "),\nand were not replaced:\n\n",
#'                   sep="")
#'     cat(msg1)
#'     cat(paste( V.names[e.source & e.target], collapse = ", "),
#'         "\n\nAdd 'rep=T' to the '",
#'         actionName[ "function" ],
#'         " command",
#'         " in order to replace these objects.\n",
#'         sep = "" )
#'   } #end 'if ((!replace.objects) & any(e.target))'
#'
#'   if (feedback & map) {
#'
#'     fdbk.dfm = data.frame(e.source,
#'                           e.target, permit.move)
#'     names(fdbk.dfm) = c( "source.obj.exists", "target.obj.exists",
#'                          paste( actionName[ "function" ], "was", "permitted", sep = "." ) )
#'     cat("\n")
#'     print( fdbk.dfm )
#'     cat("\n") }
#'
#'   n.obj.match = sum(e.source) + .001
#'   n.obj.nomatch = sum(!e.source) + .001
#'   if ( (n.obj.nomatch/n.obj.match) > 8) cat("\n",
#'                                             "The 'listObjects' argument specifies one or more object names that do not ",
#'                                             "\nmatch the names of objects in position ", src.pos,
#'                                             ".  Are you trying to ",
#'                                             actionName[ "function" ],
#'                                             "\na character vector?  If you intend to ",
#'                                             actionName[ "function" ],
#'                                             " a character vector, enclose \n",
#'                                             "its name in quotes. \n",
#'                                             sep = "")
#'
#' }
#'
#' move.doc <- "The function 'move' either moves or copies an R object from one environment on the search path to another environment on the search path.  Typically, it moves an object from .GlobalEnv to a file that is attached to the search path.  By default, it deletes the object from the environment that initially contained it, but this default can be overridden.  Also, by default, the move is not carried out if an object with the same name exists in the destination directory, but this default can also be overridden (replace.objects=T).  Character objects must be referenced as names in quotes - single non-character objects can, but need not be referenced in this way.  The move function also applies to a character vector of object names, in which case the object names must be quoted. \n\nARGUMENTS:\n\n'list' = object to be moved;  or a character vector of object names.\n\n'to' = the destination file to which the objects are to be move.  'to' can be specified as either a position, e.g., 'to = 3', or as a named environment, e.g., 'to = \"data.rda\"'.  Partial matching is used to match the named environment if an incomplete name is given. \n\n'from' = the source file or environment from which the objects are to be moved.  'from' can be specified as either a position, e.g., 'from = 3', or as a named environment, e.g., 'from = \"data.rda\"'. Partial matching is used to match the named environment if an incomplete name is given. \n\n'fn.object = FALSE' (default) means that the object to be moved is in a file on the search path (starting with .GlobalEnv).  'fn.object = TRUE' should be used only if the 'move' function is called by another function. 'fn.object = TRUE' means that the the object to be moved is within the environment created by a calling function.  In this case, the object exists in an environment that is not named by the 'search()' function.  \n\n'replace.objects' = T to replace a same name object in destination directory; If 'replace.objects' = F, then no move is carried out when a same name object exists in the destination file. \n\n'move.doc.objects = TRUE' causes XXX.doc objects to be moved along with the XXX object.  If FALSE, only the XXX object is moved.\n\nIf 'copy.only' = TRUE, then the object is copied to the destination file, but is not deleted from the source environment.  If it is FALSE, then the object is deleted after copying it to the destination file.  The default is 'copy.only = FALSE'.\n\nIf 'feedback = TRUE' (default), then feedback is printed to the screen regarding which objects have been successfully moved and which moves have failed.  If 'feedback = FALSE', this feedback is suppressed.\n\nIf 'map = TRUE', a table showing the existence and replacement of objects is displayed; if FALSE, no table is displayed. The default is 'map = TRUE'. \n"
#'
#' #' Remove object from any location on the searchpath
#' #'
#' #' @export
#' rm.sv <- function(list, env.on.path = "", pos = NA, rm.doc.objects = FALSE,
#'                   feedback = TRUE)  {
#'
#'   if (is.character(list) && length(grep(" ", list)) > 0) {
#'
#'     # # The function stops if 'list' is a string variable that appears to be a single message rather than a list of names.
#'     msg <- paste(
#'       "\nError in call to 'rm.sv' function: \n",
#'       "The 'list' argument appears to be a character string rather\n",
#'       "than a variable name or vector of variable names.  Try putting\n",
#'       "the name of the 'list' object in quotes and then run 'rm.sv' again.",
#'       "\n\n",
#'       "Warning: No object was deleted because it was not clear what object\n",
#'       "to delete.")
#'     stop(msg)
#'   } #end 'if (is.character(list) && length(grep(" ", list)) > 0)'
#'
#'   # V.names is the vector of names of objects to be removed.
#'   if (mode(list) != "character")  V.names <- deparse(substitute(list)) else V.names <- list
#'   # V.names.ini preserves this initial vector of names.  Later, the .doc object names will be added to this vector.
#'   V.names.ini = V.names
#'
#'   if (rm.doc.objects) {
#'     tm.names <- NULL
#'     for (i in 1:length(V.names)) {
#'       tm.names <- c(tm.names, V.names[i])
#'       if (exists(paste(V.names[i], '.doc', sep=''))) {
#'         tm.names <- c(tm.names, paste(V.names[i], '.doc', sep=''))
#'       } #end 'if (exists(paste(V.names[i], '.doc', sep='')))'
#'     } #end 'for (i in 1:length(V.names)'
#'     V.names <- tm.names
#'   } #end 'if (rm.doc.objects)'
#'
#'   # # Stop if 'env.on.path' and 'pos' arguments are both unspecified.
#'   if (is.na(pos) & env.on.path == "")
#'     stop(paste("\nError in call to 'rm.sv' function: \n",
#'                "You must specify either 'env.on.path' or 'pos' ",
#'                "as the location for the deletion."))
#'
#'   env.pos <- NA
#'   # # Convert 'env.on.path' to numeric position if 'env.on.path' is non-null.
#'   if (env.on.path != "") {
#'
#'     # # Stop if 'env.on.path' is not a character string.
#'     if (!is.character(env.on.path)) {
#'       msg <- paste(
#'         "\nError in the call to the 'rm.sv' function:\n",
#'         "Argument 'env.to.pos' must be a character string.\n",
#'         "No deletion was performed!")
#'       stop(msg)
#'     } #end 'if (!is.character(env.on.path))'
#'
#'     env.pos <- grep(env.on.path, search(), ignore.case=TRUE)
#'
#'     # # Stop if 'env.on.path' string is not contained within a name on the search path.
#'     if (length(env.pos) == 0) {
#'       msg <- paste(
#'         "\nError in the call to the 'rm.sv' function:\n",
#'         "'env.on.path' string not found within search path names.\n",
#'         "No deletion was performed!")
#'       cat("search()\n")
#'       print(search())
#'       cat("\n")
#'       stop(msg) }
#'
#'     # # Stop if 'env.on.path' string is contained within more than one name on the search path.
#'     if (length(env.pos) > 1) {
#'       msg <- paste(
#'         "\nError in the call to the 'rm.sv' function:\n",
#'         "More than one match found for 'env.on.path' among",
#'         "search path names.\n",
#'         "No deletion was performed!")
#'       print(search())
#'       cat("\n")
#'       stop(msg) }
#'   } #end 'if (env.on.path != "")'
#'
#'   # # Stop if 'env.on.path' and 'pos' specify different positions.
#'   if (!is.na(pos) && env.on.path != "" && pos != env.pos) {
#'     msg <- paste(
#'       "\nError in the call to the 'rm.sv' function:\n",
#'       "The 'pos'and 'env.on.path' arguments imply different positions.\n",
#'       "Only one of these arguments needs to be specified.\n",
#'       "Check these arguments and try again.\n",
#'       "No deletion was performed!")
#'     print(search())
#'     cat("\n")
#'     stop(msg)
#'   } #end 'if (!is.na(pos) && env.on.path != "" && pos != env.pos)'
#'
#'   # # Assign the correct number to env.pos = pos, if it is not already assigned.
#'   if (is.na(pos)) pos <- env.pos
#'   # # Assign the correct file name to the file from which object(s) will be deleted.
#'   tm.target <- search()[pos]
#'   target.name <- substring(tm.target, 6, nchar(tm.target))
#'
#'   #--------------------------------------------------
#'   # e.target[i] is the TRUE when V.names[i] exists in the target environment.
#'   e.target <- sapply(V.names, function(x) exists(x, where = pos, inherits = F))
#'
#'   # # The next if carries out the deletion, if this is possible.
#'   remove(list=V.names[e.target], pos = pos)
#'
#'   # Assign the correct directory name to dirN.
#'   tms <- search()[pos]
#'   dirN <- substring(tms, 6, nchar(tms))
#'   save(list=objects(pos=pos, all=T), file=dirN )
#'
#'   #--------------------------------------------------
#'   # # The next code gives feedback to the user.
#'   if (feedback & any(e.target)) {
#'     msg <- paste("The following objects were removed from position ",
#'                  pos, " ('", target.name, "'):\n     ", sep="")
#'     cat(msg)
#'     cat(V.names[e.target], "\n\n")
#'   } #end 'if (any(e.target))'
#'
#'   # original.names[i] is TRUE iff V.names[i] is among the object names that were specified in
#'   # the call to rm.sv.
#'   original.names = (V.names %in% V.names.ini)
#'   # remove.failures[i] is TRUE if V.names[i] was not removed and it was among the object names
#'   # that were specified in the call to rm.sv.
#'   remove.failures = (!e.target & original.names)
#'
#'   if (feedback & any(remove.failures)) {
#'     msg <- paste("WARNING: Some of the deletions could not be carried out!\n",
#'                  "The following objects could not be found in position ",
#'                  pos, " ('", target.name, "'):\n     ", sep="")
#'     cat(msg)
#'     cat(V.names[!e.target], "\n\n")
#'   } #end 'if (any(e.target))'
#'
#' }
#'
#' rm.sv.doc <- "\nThe function 'rm.sv' deletes an R object from an environment on the search path, and then saves that environment to its associated file.  The rm.sv function also applies to the objects specified by a character vector of object names.  \n\nArguments:\n\nlist = the object to be deleted; or a character vector of object names.\n\n'env.on.path' = the name of R-environment containing the object;  Specify 'pos' or 'env.on.path', but not both. Note that 'env.on.path' has the form, 'env.on.path = \"c:/...\" or 'env.on.path = \"c:\\\\...\", and NOT as, 'env.on.path = \"file:c:\\...\", i.e., directories on a path are separated by forward slashes or double backslashes and not by single backslashed. Furthermore, 'env.on.path' is matched by partial matching, so that 'env.on.path = \"myfuns.rda\" will be matched to \"file:c:/job/myfuns.rda\" if it is on the search path (and a unique match). Note also that 'env.on.path' and 'pos' may NOT specify a package. \n\npos = position number of the directory containing the object; by default, pos = NA. The environment where the object is to be deleted should be specified by 'env.on.path' or by 'pos', but not both.  \n\nIf 'rm.doc.objects = TRUE', then .doc objects that correspond to objects in 'list' will also be deleted.  If 'rm.doc.objects = FALSE', then only the objects names in 'list' will be deleted.  \n\n'feedback = TRUE' (default) means that the call to 'rm.sv' will give feedback about the success or failure of the move.  'feedback = FALSE' omits this feedback.\n"
#'
#' on.jm.cmptr <- function(x) FALSE
#'
#' path.std <- function(path) {
#'   #---+---+---+---+---+--
#'
#'   out = NULL
#'   #
#'   for (ii in 1:length(path)) {
#'     # If path[ii] is a file name without a path, append it to 'out'.
#'
#'     if (is.na(path[ii])) out = c(out, NA) else {
#'       if (dirname(path[ii]) == '.') out = c(out, path[ii]) else {
#'         # Else: path[ii] is not a file name without a path.
#'         # If path[ii] ends in a / or \\, save "/" to a variable.
#'         lastChar = substr( path[ii], nchar(path[ii]), nchar(path[ii]) )
#'         if (lastChar == "/" | lastChar == "\\") endChar = "/" else endChar = ""
#'         # dnp = dirname(path[ii]) is the path up to but not including the last element on the path.
#'         dnp = dirname(path[ii])
#'         bnp = basename(path[ii])
#'
#'         # if dnp = "/" or dnp = "e:/" (or possibly some other drive letter), then the new path+filename
#'         # should be paste( paste(dnp, bnp, sep = "/"), endChar, sep = ""); otherwise, the new path+filename should be
#'         # paste( paste(dnp, bnp, sep = ""), endChar, sep = "")
#'         nc.dnp = nchar( dnp )
#'         dnp.23 = substr(dnp, 2, 3)
#'
#'         if ( dnp == "/" | ( nc.dnp == 3 & dnp.23 == ":/" ) )
#'           x.sep = "" else x.sep = "/"
#'
#'         out = c(out, paste(paste( dnp, bnp, sep= x.sep ), endChar, sep = "") )
#'       }
#'
#'     }
#'
#'   } #end 'for'
#'
#'   return(out)
#' }
#'
#' path.curr <- function( filepath )  {
#'
#'   # The 'path.std' converts a path specification to a standard notation.  Specifically, 'c:\\aa\\bb.txt'
#'   # is transformed to 'c:/aa/bb.txt'.  This means that we can assume that '/' is the separator between
#'   # successive directories in the path specification.
#'   filepath.ini = path.std( filepath )
#'   #---+---+---+---+---+
#'   # JM uses computers on which "e:/", "b:/ab/john/" and "c:/temp/jmm" are the root directories
#'   # (of course, not all at the same time).  The following code removes these initial strings,
#'   # if they are present.
#'
#'   fp.unchanged = rep( TRUE, length(filepath.ini) )
#'   filepath.alt = rep( "unspecified", length(filepath.ini) )
#'
#'   for ( i in 1:length(filepath.ini) ) {
#'
#'     # case 1: filepath specifies a path on JM's X220 laptop.
#'     if ( tolower(substr(filepath.ini[i], 1, 2)) == "e:" ) {
#'       filepath.alt[i] = substr(filepath.ini[i], 3, nchar(filepath.ini[i]))
#'       fp.unchanged[i] = FALSE
#'     } #endIf
#'
#'     # case 2: filepath specifies a path on JM's UHouse pseudo-drive..
#'     if ( tolower(substr(filepath.ini[i], 1, 2)) == "j:" ) {
#'       filepath.alt[i] = substr(filepath.ini[i], 3, nchar(filepath.ini[i]))
#'       fp.unchanged[i] = FALSE
#'     } #endIf
#'
#'     # case 3: filepath specifies a path on JM's UHouse computer.
#'     if ( tolower(substr(filepath.ini[i], 1, 10)) == "b:/ab/john" ) {
#'       filepath.alt[i] = substr(filepath.ini[i], 11, nchar(filepath.ini[i]))
#'       fp.unchanged[i] = FALSE
#'     } #endIf
#'
#'     # case 4: filepath specifes a path on a CSSCR computer
#'     if ( tolower(substr(filepath.ini[i], 1, 11)) == "c:/temp/jmm" ) {
#'       filepath.alt[i] = substr(filepath.ini[i], 12, nchar(filepath.ini[i]))
#'       fp.unchanged[i] = FALSE
#'     } #endIf
#'
#'     # case 5: None of the above applies
#'     if (fp.unchanged[i]) filepath.alt[i] = filepath.ini[i]
#'   } #endFor
#'   #---+---+---+---+---+
#'   # The following code checks whether R is running on one of the computers that JM usually works on.
#'   # If R is running on one of JM's computers, then the path is modified.  If R is not running on one
#'   # of JM's computers, e.g., because a student is using this code on his or her own computer,
#'   # then the 'path.curr' function has no effect on the 'filepath' input.
#'   jm.cmptr = on.jm.cmptr()
#'   #---+---+---+---+---+
#'   # There are two cases:
#'   #
#'   # Case 1:  'filepath.alt' does not start with "/".  This means that 'filepath.alt' designates a file in the current
#'   # working directory or it it designates the relative path to a file that starts at the current working directory.
#'   # In this case, the output of 'path.curr' should be the same as 'filepath.alt'.
#'   #
#'   # Case 2:  'filepath.alt' does contain "/".  In this case the full path from the root directory on the
#'   # current computer should be added to the file specification.
#'   # The following code takes care of these two cases.
#'
#'   # Case 1:  Case 2 will alter 'res' if Case 1 does not apply.
#'   res = filepath
#'
#'   # The following 'if-else' clause alters the 'filepath' in the case where R is running on one of
#'   # JM's computers, or it leaves it unchanged in the case where R is running on any other computer.
#'   if (jm.cmptr) {
#'     #---+---+---+---+---+
#'     slash.ini = (substr(filepath.alt, 1, 1) == "/")
#'
#'     # Case 2 will change 'res' to something else.
#'
#'     res = ifelse(
#'       test = slash.ini,
#'       yes  = paste( root.dir(),
#'                     substr( filepath.alt, 2, nchar(filepath.alt) ),
#'                     sep = ""),
#'       no   = filepath.alt )
#'
#'   } else {  #end 'if (jm.cmptr)'
#'     if (exists("localRoot"))
#'       slash.ini = (substr(filepath.alt, 1, 1) == "/")
#'
#'     res = ifelse(
#'       test = slash.ini,
#'       yes  = paste( localRoot, filepath.alt, sep = ""),
#'       no   = filepath.alt )
#'   } #end 'else for 'if (jm.cmptr)'
#'
#'   if (!jm.cmptr & !exists("localRoot")) {
#'     cat("\nThe path.curr function was called with the input:",
#'         "\n     ", filepath, ";",
#'         "\nIt returned the value:",
#'         "\n     ", res,
#'         "\nbecause the current R session is not on one of JM's computers,",
#'         "\nand no localRoot is specified. If this is an error, specify a value",
#'         "\nfor the localRoot.")
#'   } #endIf
#'
#'   return(res)
#'
#' }
#'
#'
#' ##' @export
#' attach_and_check <- function(
#'   filename,
#'   pos = 2,
#'   allow.2same = FALSE,
#'   refresh.path = FALSE,
#'   case.sensitive = FALSE,
#'   name.on.path = character(0),
#'   feedback = TRUE
#' ) {
#'   #---+---+---+---+---+--
#'   # 'filename' is either a length=1 character string that names a file (possibly with path) or
#'   # a list, dataframe. Establish which case we are dealing with.
#'   f.is.list <- is.list(filename)
#'   #---+---+---+---+---+--
#'   # The attach function does not apply to a matrix.
#'   if (!f.is.list & is.matrix(filename)) stop(
#'     "\nThe 'filename' argument was assigned to a matrix.  A matrix cannot be ",
#'     "\nbe attached to the search path.  Therefore the 'attach.jm' function will ",
#'     "\nexit without taking any action. "
#'   ) #end stop
#'   #---+---+---+---+---+--
#'   # nameString is the name of the object or file that is assigned to filename.
#'   if (f.is.list) nameString = deparse(substitute(filename)) else nameString = filename
#'   #---+---+---+---+---+
#'   # 'File.Exists' flags if a file named 'filename' exists on the computer.
#'   File.Exists = FALSE
#'   #---+---+---+---+---+
#'   # File.Exists is true if there exists a file by the name of 'filename' on the current computer.
#'   if ( length(filename) == 1 && is.character(filename) ) {
#'     File.Exists = file.exists( path.curr(filename) )
#'   } #end 'if ( length(filename) == 1 && is.character(filename) )'
#'   #---+---+---+---+---+
#'   # Issue a warning and stop processing if 'filename' is neither the name of a list or matrix
#'   # nor does it name a file on this computer.
#'   if (!f.is.list & !File.Exists) {
#'     tmMsg = paste(
#'       deparse(substitute(filename)),
#'       " does not name an existing list,\n",
#'       "dataframe, matrix or file. Do you want to create a file\n",
#'       "named ", filename, "?" , sep = "")
#'   } #endIf
#'   #---+---+---+---+---+
#'   # Issue a warning and stop processing if 'filename' is the name of a list or matrix
#'   # and it also names a file on this computer.
#'   if (f.is.list & File.Exists) stop("\n",
#'                                     "An object named '", filename, "' exists on the R search path and \n",
#'                                     "a file named '", filename, "' also exists on the computer (probably,\n",
#'                                     "although not necessarily, in the current working directory).  Change\n",
#'                                     "the name of the object or the file name if you want to use 'attach.jm'\n",
#'                                     "to attach it to the search path.\n"
#'   ) #end stop
#'   #---+---+---+---+---+
#'   flag.attach <- FALSE
#'   # 'flag.attach' will be set to TRUE if  'attach.jm' attaches something to the search path (below).
#'   attachType = "nothing"
#'   # attachType will be reset to a descriptive label, e.g., "file", to describe the object that is attached.
#'   #---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-
#'   # The next 'if' deals with the case that 'filename' is a list, dataframe or matrix (not the name of a file).
#'   if (f.is.list) {
#'     # The next 'if' sets 'name.on.path' to the name of the 'filename' object in the case
#'     # where 'filename' is a list, dataframe or matrix and 'name.on.path' is set to 'character(0)'
#'     # in the function call.
#'     if (length(name.on.path) == 0) name.on.path <- deparse(substitute(filename))
#'
#'     FileOnSearchPath <- any(name.on.path == search())
#'     #---+---+---+---+---+
#'     if (!FileOnSearchPath | allow.2same) {
#'       attach(filename, pos=pos, name = name.on.path, warn.conflicts = FALSE)
#'       flag.attach <- TRUE
#'       attachType = "list.or.matrix"
#'     } else { #end 'if (!FileOnSearchPath | allow.2same)'
#'       #---+---+---+---+---+
#'       # This 'else' clause deals with the case where 'FileOnSearchPath = TRUE & allow.2same = FALSE'.
#'       # The next 'if' attaches the list or dataframe, 'filename', in the case where it is already on the search path,
#'       # but 'refresh.path = TRUE', hence,  the existing list or dataframe is detached and a new copy if attached.
#'       if (refresh.path) {
#'         # Remove older environments named 'name.on.path' from the search path.
#'         det.jm(filename, all.matches = TRUE, feedback = FALSE)
#'         # Attach a fresh version of 'filename' under the name, 'name.on.path'.
#'         attach(filename, pos=pos, name = name.on.path, warn.conflicts = FALSE)
#'         flag.attach <- TRUE
#'         attachType = "list.or.matrix"
#'       } #end 'if (FileOnSearchPath & refresh.path)'
#'       # Give warning if no attachments were carried out.
#'       if (!refresh.path & feedback) warning(
#'         paste(
#'           "A call was made to 'attach.jm' to attach '", name.on.path,
#'           "' to the search path.  However, \n",
#'           "an object named '", name.on.path, "' is already on the search ",
#'           "path.  Either set\n'allow.2same = TRUE' or 'refresh.path = ",
#'           "TRUE' or detach this ",
#'           "object\nbefore running 'attach.jm'.  Nothing was attached.\n",
#'           sep="")  )
#'     }  #end of 'else' for 'if (!FileOnSearchPath) | allow.2same)
#'     #---+---+---+---+---+
#'
#'     #End of case where 'filename' is a list (dataframe) or matrix.
#'     #---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-
#'   } else   #end of 'if (f.is.list)'
#'   {  #start of 'else' clause for 'if (f.is.list)'
#'     # Start of case where 'filename' is the name of a file.
#'     # This is the start of the case where 'filename' is a string that names a file
#'     # to be attached to the search path.  Note: The 'path.std' function creates a standardized
#'     # version of any path specification, i.e., if any '\\' are in the path specification, they are
#'     # replaced with '/'.  In this way, we don't have to worry about the fact that
#'     # 'c:\\data.rda' and 'c:/data.rda' are equivalent file specifications. The file specification
#'     # and search path are set to lower case when 'case.sensitive' equals FALSE.
#'
#'     filename.std <- path.std( path.curr(filename) )
#'     search.std <- sapply(search(), path.std)
#'
#'     root.ref   = paste("file:", root.dir(), sep = "")
#'     search.std = sub("file:/", root.ref, search.std)
#'
#'     searchname.std <- paste("file:", filename.std, sep="")
#'
#'     if (!case.sensitive) searchname.std <- tolower(searchname.std)
#'     if (!case.sensitive) search.std <- tolower(search.std)
#'
#'     names(search.std) <- NULL
#'     # Next create a flag for whether the file is already attached to the search path.
#'     File.Attached <- any(searchname.std == search.std)
#'     # The next case detaches a previously attached copy of the file prior to attaching it again.
#'     if (File.Attached & !allow.2same) {
#'       if (refresh.path) {
#'         det.jm( path.curr(filename), all.matches = TRUE, feedback = FALSE)
#'
#'         # The following attach was added on 7/21/2011.  I believe it was mistakenly omitted
#'         # in previous versions of this function, but watch for whether it causes errors.
#'         attach( path.curr(filename), pos=pos, warn.conflicts = FALSE)
#'         attachType = "file"
#'         #---+---+---+---+---+
#'         search.std <- sapply(search(), path.std)
#'         if (!case.sensitive) search.std <- tolower(search.std)
#'         names(search.std) <- NULL
#'         File.Attached <- any(searchname.std == search.std)
#'       } #end 'if (refresh.path)'
#'     } else {   #end 'if (File.Attached & !allow.2same)'
#'       # This 'else' clause encompasses the case where 'File.Attached = FALSE' or 'allow.2same = TRUE'
#'
#'       # The next 'if' attaches the file in the case where the file is already on the search path but
#'       # 'allow.2same = TRUE', i.e., two files with identical names are allowed.
#'       if (File.Attached & allow.2same) {
#'         attach( path.curr(filename), pos=pos, warn.conflicts = FALSE)
#'         flag.attach <- TRUE
#'         attachType = "file"
#'       } #end 'if (File.Attached & allow.2same)'
#'     } #end 'else' for 'if (File.Attached & !allow.2same)'
#'     # The next case attaches a file that is not present on the search path.
#'     if (!File.Attached) {
#'
#'       attach( path.curr(filename) , pos=pos, warn.conflicts = FALSE)
#'       flag.attach <- TRUE
#'       attachType = "file"
#'
#'     }  else  {  #end of 'if (!File.Attached)'
#'
#'       if (!flag.attach & feedback) warning(paste("\n\nThe file 'file:",
#'                                                  path.curr(filename),
#'                                                  "' is already attached\nto the search path.  The current ",
#'                                                  "'attach.jm' command had no effect.\n",
#'                                                  "Set 'allow.2same' to TRUE to allow 2 copies of the same file\n",
#'                                                  "on the search path.\n", sep=""))
#'
#'     }  #end of 'else' for 'if (File.Exists & !File.Attached)'
#'     #End of case where 'filename' is the name of a file.
#'
#'   }  #end of 'else' for 'if (f.is.list)'
#'   #---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-
#'   if (feedback & flag.attach) {
#'     cat("\nCurrent search path is:\n")
#'     print(search())
#'
#'     if (exists( "conflicts.jm" )) {
#'       tm.t <- conflicts.jm(detail = TRUE, all.nm = T)
#'
#'       # attachType = "list.or.matrix" or "file" ("nothing" is excluded because flag.attach == TRUE)
#'       # The following code corrects an error that used to occur when 'filename' was set to a dataframe (pre-2/12/2014).
#'       # Note that path.curr(nameString) returns nameString if nameString is the name of a dataframe.
#'       cfl.LOC.1 = (path.curr(nameString) == tm.t$position.1)
#'       cfl.LOC.2 = (path.curr(nameString) == tm.t$position.2)
#'
#'
#'       confl = (cfl.LOC.1 | cfl.LOC.2)
#'
#'       tm.T = tm.t[confl,]
#'
#'       if (nrow(tm.T) > 0) {
#'         cat(
#'           "\nWARNING: Attaching ", nameString,
#'           " to the search path creates conflicts ",
#'           "\nbetween the following object names: \n", sep = "")
#'
#'         print(tm.T)
#'
#'       } #endIf
#'
#'     } #endIf
#'
#'   } #endIf: if (feedback & flag.attach) ...
#'
#'   if (feedback & !flag.attach) {
#'     ss <- search()
#'     if (length(ss) < 11) search.vec <- ss else {
#'       if (round(length(ss)/2, digits = 0) != length(ss)/2) c(ss, "---") }
#'     tm.v <- paste(1:length(ss), ": ", ss, sep="")
#'     if ((length(tm.v) %% 2) == 1) tm.v <- c(tm.v, "")
#'     search.mat <- matrix(tm.v, ncol=2)
#'     tmm <- max(nchar(search.mat[,1])) + 3
#'     out.vec <- NULL
#'     for (i in 1:length(search.mat[,1])) {
#'       out.vec <- c(out.vec, paste(
#'         search.mat[i,1],
#'         paste(rep(' ', tmm - nchar(search.mat[i, 1])), collapse=""),
#'         search.mat[i,2], "\n",
#'         collapse = "") #end of 'paste'
#'       ) #end of 'c'
#'     } #end 'for (i in 1:length(search.mat[,1]))'
#'
#'     warning("\nCurrent search path is:\n", out.vec)
#'
#'   } #end 'if (feedback & !flag.attach)'
#'   #---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-
#' }
