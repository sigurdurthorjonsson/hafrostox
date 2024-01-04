#' Checks if 'projectname' is an RStoX project.
#'
#' Checks if 'projectname' is an RStoX project.
#'
#' @export
#'
isProject <- function(projectName, subset.out=FALSE){
        # The following is done in getProjectPaths():
        #       1. Look for the project if given by the full path
        #       2. Look for the project in the default root and sub directories
        # Function for checking whether all the folders given in getRstoxEnv()$StoX_data_sources are present in the directory:
        hasStoX_data_sourcesOne <- function(projectName){
                suppressWarnings(projectName <- getProjectPaths(projectName)$projectPath)
                # If the project paths do not exist:
                if(is.na(projectName[1])){
                        return(FALSE)
                }
                # Verify that the projectInfo is a directory with the required sub directories:
                projectInfo <- file.info(projectName)
                if(isTRUE(projectInfo$isdir)){
                        dirs <- list.dirs(projectName, full.names=FALSE, recursive=FALSE)
                        if(all(getRstoxDef("StoXFolders") %in% dirs)){
                                return(TRUE)
                        }
                        else{
                                #warning(paste0("The path ", projectName, " does not contain the required folders (", paste(getRstoxEnv()$StoX_data_sources, collapse=", "), ")"))
                                return(FALSE)
                        }
                }
                else{
                        return(FALSE)
                }
        }

        hasStoX_data_sources <- function(x, subset.out=FALSE){
                out <- unlist(lapply(x, hasStoX_data_sourcesOne))
                if(subset.out){
                        x[out]
                }
                else{
                        out
                }
        }

        ### # Check first the 'projectName' directly (which needs to be a full path, indicated by the !dirname(projectName) %in% c(".", "", "/")):
        ### out <- FALSE
        ### if(!dirname(projectName) %in% c(".", "", "/")){
        ###     out <- hasStoX_data_sources(projectName)
        ### }
        ### # Then look for the project in the default workspace:
        ### if(!out){
        ###     out <- hasStoX_data_sources(getProjectPaths(projectName)$projectPath)
        ### }
        ### out

        hasStoX_data_sources(projectName, subset.out=subset.out)
}

#' Get StoX project.
#' 
#' Get StoX project.
#'
#' @export
#'
getProject <- function(projectName, out=c("project", "baseline", "baseline-report", "name"), msg=FALSE){
        # Return immediately if a project or baseline object is given:
        if(class(projectName) == "jobjRef"){
                if(projectName@jclass=="no/imr/stox/model/Project"){
                        project <- projectName
                }
                else if(projectName@jclass=="no/imr/stox/model/Model"){
                        project <- projectName$getProject()
                }
        } 
        # Check for the existence of the project object in the RstoxEnv evnironment (getProjectPaths(projectName)$projectName assures that the project name is used and not the full project path if given in 'projectName'):
        #else if(is.character(projectName) && nchar(projectName)>0 && length(getRstoxEnv()[[getProjectPaths(projectName)$projectName]]$projectObject)>0){
        else if(is.character(projectName) && nchar(projectName)>0){
                # Get the project paths from the projectName:
                projectPaths <- getProjectPaths(projectName)
                
                # List open projects:
                projectList <- listOpenProjects()
                
                #projectName <- temp$projectName
                #projectPath <- temp$projectPath
                #if(length(getRstoxEnv()$Projects[[projectName]]$projectObject)){
                if(projectPaths$projectName %in% projectList$projectName){
                        if(msg || !projectPaths$projectPath %in% projectList$projectPath){
                                warning(paste0("Project retrieved from RstoxEnv$Projects[['", projectName, "']]. To reopen the project use reopenProject(", projectName, ")"))
                        }
                        #project <- getRstoxEnv()$Projects[[projectName]]$projectObject
                        project <- getRstoxEnv()$Projects[[projectPaths$projectName]]$projectObject
                }
                else{
                        return(NULL)
                }
                #return(getRstoxEnv()[[projectName]]$projectObject)
        }
        else{
                return(NULL)
        }
                
        # Get the StoX Java function:
        modelTypeJavaFun <- getModelType(out[1])$fun
        
        # Return either the project object itself, or the funciton identified using modelTypeJavaFun():
        if(startsWith(tolower(out[1]), "project")){
                return(project)
        } 
        else if(is.na(modelTypeJavaFun)){
                warning("Invalid value of 'out'")
                return(NULL)
        }                       
        else{                   
                return(rJava::.jrcall(project, modelTypeJavaFun))
        }                       
                                
                        
        #       
        ## Return a baseline object:
        #if(startsWith(tolower(out[1]), "baseline")){
        #       return(project$getBaseline())
        #}
        ## Return the baseline report object:
        #if(startsWith(tolower(out[1]), "report")){
        #       return(project$getBaselineReport())
        #}
        ## Return the project object:
        #else if(startsWith(tolower(out[1]), "project")){
        #       return(project)
        #}
        ## Return the project name:
        #else if(startsWith(tolower(out[1]), "name")){
        #       return(project$getProjectName())
        #}
        #else{
        #       warning("Invalid value of 'out'")
        #       return(NULL)
        #}
}
 
#*********************************************
#*********************************************
#' Initialize rJava
#'
#' This funcion initializes the connection to Java.
#'
#' @import Rstox
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#'
#' @importFrom rJava .jpackage
#' @export
#' @keywords internal
#'
Rstox.init <- function() {
        # Package initialization of rJava. Note that the documentatino of this functions also contains importing of the four packages grDevices, graphics, stats and utils. This is a bit of cheating, but avoids using :: for such common functions.
        pkgname <- "Rstox";
        loc = dirname(path.package(pkgname))
        # rJava - load jar files in package java directory
        rJava::.jpackage(pkgname, lib.loc=loc)
}


#' Get RStox baselin
#'
#' Get RStox baselin
#'
#' @export
#'
getBaseline <- function(projectName, input=c("par", "proc"), proc="all", drop=TRUE, startProcess=1, endProcess=Inf, reset=FALSE, save=FALSE, modelType="baseline", msg=TRUE, exportCSV=FALSE, warningLevel=0, parlist=list(), tempRScriptFileName=NULL, close=FALSE, ...){

        # Locate/run the baseline object. If rerun=TRUE or if parameters are given different from the parameters used in the last baseline run, rerun the baseline, and if the :
        #baseline <- runBaseline(projectName, startProcess=startProcess, endProcess=endProcess, reset=reset, save=save, out="baseline", msg=msg, parlist=parlist, ...)
        baseline <- runBaseline(projectName, out=modelType[1], startProcess=startProcess, endProcess=endProcess, reset=reset, save=save, modelType=modelType[1], msg=msg, exportCSV=exportCSV, warningLevel=warningLevel, tempRScriptFileName=tempRScriptFileName, parlist=parlist, ...)

        if(msg){
                cat("Reading:\n")
        }
        # Get the processes of the baseline/baseline report and match with those given in 'proc':
        processes <- getBaselineParameters(projectName, modelType=modelType[1])$last # Use out=modelType
        processNames <- names(processes)
        ### functionNames <- sapply(processes, "[[", "functionName")
        matchedProcesses <- processNames[getProcess(projectName, proc=proc, modelType=modelType[1])]

        ######################################################
        ##### (1) Get a list of processes with paramers: #####
        if("par" %in% input){
                input <- c(processNames, input)
        }
        # Using input = FALSE, NULL, or "" suppresses returning parameters of the baseline:
        if(!any(identical(input, FALSE), length(input)==0, nchar(input)==0)){
                if(msg){ cat("Baseline parameters\n")}
                parameters <- processes[intersect(input, processNames)]
        }
        else{
                parameters <- NULL
        }
        ######################################################

        ###########################################
        ##### (2) Get output from processes: #####
        if(length(matchedProcesses)){
                outputData <- lapply(matchedProcesses, function(xx) {if(msg) {cat("Process output", xx, "\n")}; suppressWarnings(getDataFrame(baseline, processName=xx))})
                names(outputData) <- matchedProcesses
        }
        else{
                outputData <- NULL
        }
        ###########################################

        ###########################################
        ##### (3) Get a list of process data: #####
        project <- openProject(projectName, out="project")
        # Changed made on 2019-01-11 after note from Mikko Vihtakari, who had installed Java 11 due to endless problems getting Java 8 and rJava to work. Java 11 seems to have introduced a difference in the behavior of the toArray() function, where the output is not a character vector but a Java Array object. For this reason the utility jobjRef2Character() was created, which converts to string first and then parses the string into a vector:
        # processdataNames <- project$getProcessData()$getOutputOrder()$toArray()
        processdataNames <- jobjRef2Character(project$getProcessData()$getOutputOrder())

        if("proc" %in% input){
                input <- c(processdataNames, input)
        }
        processdataNames <- intersect(processdataNames, input)
        processData <- lapply(processdataNames, function(xx) {if(msg) {cat("Process data", xx, "\n")}; suppressWarnings(getProcessDataTableAsDataFrame(projectName, xx))})
        names(processData) <- processdataNames
        ###########################################

        # Return the data:
        out <- list(parameters=parameters, outputData=outputData, processData=processData)
        if(drop){
                out <- out[sapply(out, length)>0]
                while(is.list(out) && length(out)==1){
                        out <- out[[1]]
                }
        }

        if(close){
                closeProject(projectName)
        }

        invisible(out)
}

#*********************************************
#*********************************************
#' Run a StoX baseline model
#'
#' \code{runBaseline} runs a StoX baseline model possibily overriding parameters. \cr \cr
#' \code{getBaseline} returns input and output data from the StoX baseline model. \cr \cr
#'
#' The parameters startProcess and endProcess specify the range of processes to run (startProcess : endProcess). If the model has been run already for all or some of the processes in this range, only the unrun processes are run. If there are processes in or prior to this range for which parameters have changed, all processes from the first changed process and throughout the range startProcess : endProcess are rerun. The range of processes to run is extended to any changed processes beyond the range. If reset=TRUE, the range or processes to run is extended to the range startProcess : endProcess regardless of whether the processes have previouslu been run.
#'
#' @param projectName                   The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param startProcess                  The name or number of the start process in the list of processes in the model (run \code{\link{runBaseline}} to get the processes of the project). The use of startProcess and endProcess requres that either no processes in the given range of processes depends on processes outside of the range, or that a baseline object is given in the input.
#' @param endProcess                    The name or number of the end process in the list of processes in the model.
#' @param reset                                 Logical; if TRUE rerun the baseline model even if it has been run previously.
#' @param save                                  Logical; if TRUE changes to the project specified in parlist and "..." are saved in Java and to the object javaParameters in the project list in the RstoxEnv environment.
#' @param out                                   The object to return from runBaseline(), one of "name" (projectName), "baseline" (Java baseline object) or "project" (Java project object, containing the baseline object). First element used.
#' @param modelType                             The type of model to run, currently one of "baseline" and "baseline-report". First element used.
#' @param msg                                   Logical: if TRUE print information about the progress of reading the data.
#' @param exportCSV                             Logical: if TRUE turn on exporting csv files from the baseline run.
#' @param warningLevel                  The warning level used in the baseline run, where 0 stops the baseline for Java warnings, and 1 continues with a warning.
#' @param tempRScriptFileName   A file name specifying the temporary R script that StoX writes and sources e.g. for stratum area calculation using the accurate method.
#' @param parlist                               List of parameters values overriding existing parameter values. These are specified as processName = list(parameter = value), for example AcousticDensity = list(a = -70, m = 10), BioStationWeighting = list(WeightingMethod = "NASC", a = -70, m = 10). Logical parameters (given as strings "true"/"false" in StoX) can be given as logical TRUE/FALSE.
#' @param ...                                   Same as parlist, but can be specified separately (not in a list but as separate inputs).
#' @param input                                 The input data requested in getBaseline(). This is a string vector naming baseline processes and process data. The key words "par" and "proc" returns all parameters and process data, respectively.
#' @param proc                                  A string vector naming processes from which data should be returned.
#' @param par                                   A list of the same length as \code{fun} giving parameter values to uniquely identify processes. The list names are the names of the baseline process parameters, and the values are the baseline process values.
#' @param drop                                  Logical: if TRUE drop empty list elements (default).
#' @param close                                 Logical: if TRUE close the project on exit of the function (for \code{getBaseline}).
#' @param fresh                                 Logical: if TRUE write new r.R and r-report.R scripts to the folder output/r.
#' @param add.time                              Logical: if TRUE add the current time to messages printed to the console in \code{runRScripts}.
#'
#' When a StoX project has been run using \code{runBaseline} or \code{getBaseline}, the Java object of the project is saved in the project environment, see names(RstoxEnv$Projects). If there are changes made in the project, e.g., replaced files or manual changes in the project.xml file
#'
#' @return For \code{\link{runBaseline}} theproject name, and for \code{\link{getBaseline}} a list of three elements named "parameters", "outputData", "processData", where empty elements can be dropped.
#'
#' @export
#' @rdname runBaseline
#' 
runBaseline <- function(projectName, out=c("project", "baseline", "baseline-report", "name"), startProcess=1, endProcess=Inf, reset=FALSE, save=FALSE, modelType="baseline", msg=TRUE, exportCSV=FALSE, warningLevel=0, tempRScriptFileName=NULL, parlist=list(), ...){

        # Function for extracting a range covering a series of ranges:
        setProcRange <- function(...){
                l <- unlist(list(...))
                if(length(l)==0){
                        return(NULL)
                }
                else{
                        temp <- range(l)
                        do.call(seq, as.list(temp))
                }
        }

        # Open the project (avoiding generating multiple identical project which demands memory in Java):
        #projectName <- getProjectPaths(projectName)$projectName
        # If reset==TRUE allow for the warning in getProject():

        if(length(projectName)){
                #baseline <- openProject(projectName, out="baseline")
                baseline <- openProject(projectName, out=modelType[1])
        }
        else{
                warning("Empty 'projectName'")
                return(NULL)
        }

        # Set the tempRScriptFileName if specified. This can be used when running getBaseline() in parallel, to ensure that each run writes and sources unique R scripts:
        if(length(tempRScriptFileName) && is.character(tempRScriptFileName)){
                setTempRScriptFileName(projectName, tempRScriptFileName, msg=msg)
        }

        # Avoid breaks in the baseline, and set the warning level (used in bootstrapping to ease warnings):
        baseline$setBreakable(jBoolean(FALSE))
        baseline$setWarningLevel(jInt(warningLevel))

        # If exportCSV==TRUE, reset the baseline (changed on 2018-08-28):
        if(!exportCSV){
                baseline$setExportCSV(jBoolean(FALSE))
        }
        else{
                cat("Exporting CSV files (rerunning baseline)...\n")
                baseline$setExportCSV(jBoolean(TRUE))
                reset <- TRUE
        }

        # Remove processes that saves the project.xml file, which is assumed to ALWAYS be the last process. Please ask Åsmund to set this as a requirement in StoX:
        #numProcesses      <- baseline$getProcessList()$size() - length(baseline$getProcessByFunctionName("WriteProcessData"))
        # Change on 2018-09-21, counting the number of save processes:
        numProcesses      <- baseline$getProcessList()$size() - length(JavaString2vector(baseline$getProcessesByFunctionName("WriteProcessData")))
        currentEndProcess <- baseline$getRunningProcessIdx() + 1


        ############################################################
        ######################## Processes: ########################
        ############################################################
        # Define the unrun processes, which is NULL if the current end process is larger or equal to the number of processes (excluding the function "WriteProcessData"):
        if(currentEndProcess >= numProcesses){
                procUnrun <- NULL
        }
        else{
                procUnrun <- seq(currentEndProcess + 1, numProcesses)
        }

        # Get the processes to run:
        startProcess <- getProcess(projectName, proc=startProcess, modelType=modelType[1])
        endProcess   <- getProcess(projectName, proc=endProcess, modelType=modelType[1])
        procStartEnd <- seq(startProcess, endProcess)

        # Get the changed processes:
        # Detect changes to the baseline parameters compared to the last used parameters. This is done only to determine whether the baseline should be rerun. Using save=FALSE ensures that the parameters are not set but simply that changes are identified:
        procChanged <- setBaselineParameters(baseline, parlist=parlist, msg=FALSE, save=FALSE, ...)

        #
        #parlist <- getParlist(parlist=parlist, ...)
        #javapar <- getBaselineParameters(projectName, out=modelType[1])$java # Use out=modelType
        #newpar  <- modifyBaselineParameters(projectName, javapar, parlist=parlist)$parameters
        #lastpar <- getBaselineParameters(projectName, out=modelType[1])$last # Use out=modelType
        #
        ## Change made on 2017-09-15: If no valid processes are given in parlist or ..., using which() around the following line returned an error. which() is now moved to inside the if(any(changedProcesses)){}:
        #procChanged <- which(sapply(seq_along(newpar), function(i) !identical(newpar[[i]], lastpar[[i]])))
        ## Convert to all indices covered by the range of changed processes:
        #if(length(procChanged)){
        #       procChanged <- seq(min(procChanged), max(procChanged, endProcess))
        #}
        ############################################################


        ### # Regardless of the value of reset, we rerun the span of the range of procUnrun and procChanged. That is, if there are processes that are have not been run or have had parameters changed in memory, rerun:
        ### procTorun <- setProcRange(procUnrun, procChanged)
        ###
        ### # If reset=TRUE, we also include the processes specified through 'startProcess' and 'endProcess':
        ### if(reset){
        ###     procTorun <- setProcRange(procTorun, procStartEnd)
        ### }
        ###
        ###

        # The parameters startProcess and endProcess specify the range of processes to run (startProcess : endProcess). If the model has been run already for all or some of the processes in this range, only the unrun processes are run. If there are processes in or prior to this range for which parameters have changed, all processes from the first changed process and throughout the range startProcess : endProcess are rerun. The range of processes to run is extended to any changed processes beyond the range. If reset=TRUE, the range or processes to run is extended to the range startProcess : endProcess regardless of whether the processes have previouslu been run.


        # 1. If any of 1:End are unrun or changed, run from the first of these to End:
        if(any(c(procUnrun, procChanged) %in% seq_len(max(procStartEnd)))){
                procTorun <- seq(min(procUnrun, procChanged), max(procStartEnd))
        }
        else{
                procTorun <- NULL
        }

        # 2. If reset=TRUE, extend to the range of procStartEnd:
        if(reset){
                procTorun <- setProcRange(procTorun, procStartEnd)
        }

        # 3. If any of procChanged > procTorun, run to the max of these:
        if(any(procChanged > suppressWarnings(max(procTorun)))){
                procTorun <- setProcRange(procTorun, procChanged)
        }

        # Remove 0:
        procTorun <- procTorun[procTorun > 0]


        ### # 1. If there are unrun processes in procStartEnd, use this as the last funciton to run
        ### if(suppressWarnings(min(procUnrun)) <= max(procStartEnd)){
        ###     procTorun <- seq(min(procUnrun), max(procStartEnd))
        ### }
        ### else{
        ###     procTorun <- NULL
        ### }
        ###
        ### # 2. If reset=TRUE, extend to the range of procStartEnd:
        ### if(reset){
        ###     procTorun <- setProcRange(procTorun, procStartEnd)
        ### }
        ###
        ### # 3. If there are processes with changed parameters, expand by the range of these. The changed processes takes presedence:
        ### if(length(procChanged)){
        ###     procTorun <- setProcRange(procTorun, procChanged)
        ### }

        # Override parameters in the baseline:
        #if(run){
        if(length(procTorun)){
                startProcess <- min(procTorun)
                endProcess <- max(procTorun)
                #if(msg)        {cat("Running baseline process ", startProcess, " to ", endProcess, " (out of ", numProcesses, " processes, excluding save processes)\n", sep="")}
                if(msg) {cat("Running ", modelType[1], " process ", startProcess, " to ", endProcess, " (out of ", numProcesses, " processes)\n", sep="")}

                # If parameters are given, override the java parameters in memory, and store the java (if save=TRUE) and last used parameters:
                if(length(procChanged)){
                #if(length(parlist)){
                         # Get the java parameters for use later if save==FALSE:
                         if(!save){
                                javapar <- getBaselineParameters(projectName, modelType=modelType[1])$java # Use out=modelType
                                #javapar <- getBaselineParameters(projectName)$java # Use out=modelType
                         }
                        # Set the new parameters:
                        newpar <- setBaselineParameters(baseline, parlist=parlist, msg=FALSE, save=c("last", "java"), ...)
                        ### ACCOUNTED FOR IN setBaselineParameters() # Make sure that processes are given by the correct form "Process(processName)"
                        ### ACCOUNTED FOR IN setBaselineParameters() newpar <- getParlist(newpar)

                        # Run the baseline:
                        baseline$run(jInt(startProcess), jInt(endProcess), jBoolean(FALSE))

                        # Set the 'javaParameters' object and the parameters in Java memory back to the original:
                        if(!save){
                                setBaselineParameters(baseline, parlist=javapar, msg=FALSE, save="java")
                        }
                }
                else{
                        # Run the baseline:
                        baseline$run(jInt(startProcess), jInt(endProcess), jBoolean(FALSE))
                }
        }

        # Return the object specified in 'out':
        #return(getProject(projectName, out=out))
        return(getProject(baseline, out=out))
        ## Return a baseline object:
        #if(tolower(substr(out[1], 1, 1)) == "b"){
        #       return(baseline)
        #}
        ## Return the project object:
        #if(tolower(substr(out[1], 1, 1)) == "p"){
        #       return(baseline$getProject())
        #}
        ## Return the project name:
        #else{
        #       return(projectName)
        #}
}

#' Open StoX project.
#' 
#' Open StoX project.
#'
#' @importFrom rJava J
#' @export
#'
openProject <- function(projectName=NULL, out=c("project", "baseline", "baseline-report", "name"), msg=FALSE){
        # Old version, listing everything in the default workspace:
        #return(list.files(J("no.imr.stox.functions.utils.ProjectUtils")$getSystemProjectRoot()))
        
        # Initialize the list of available projects, and get this list only if neseccary:
        availableProjects <- NULL
         
        # If nothing is given return a list of the projects in the StoX project directory:
        if(length(projectName)==0){
                # Get the available projects, ordered from the top level and down:
                availableProjects <- getAvailableProjects()
                if(is.list(projectName)){
                        return(availableProjects$projectNameList)
                }
                else{
                        return(availableProjects$projectPaths)
                }
        } 
  
        # Get the project Java object, possibly retrieved from the project environment (getProject() uses getProjectPaths() if a character is given):
        project <- getProject(projectName, msg=msg)
        
        # Otherwise, open the project, generate the project object, and save it to the RstoxEnv evnironment:
        if(length(project)==0){
                # Search for the project:
                if(!isProject(projectName)){
                        # Get the available projects, ordered from the top level and down:
                        if(length(availableProjects)==0){
                                availableProjects <- getAvailableProjects() 
                        }
  
                        # If the Test_Rstox project is requested, create it:
                        if(identical(projectName, "Test_Rstox")){
                                temp <- createProject("Test_Rstox")
                                availableProjects$projectPaths <- c(temp, availableProjects$projectPaths)
                        }
            
                        # Get avaiable project names and match against the requested project. The top levels are prioritized in availableProjects$projectPaths:
                        availableProjectNames <- basename(availableProjects$projectPaths)
                        matches <- which(availableProjectNames %in% getProjectPaths(projectName)$projectName)
                        if(length(matches)==0){
                                warning(paste0("The StoX project \"", projectName, "\" not found in the default workspace \"", availableProjects$workspace, "\". For projects not located in this directory or its sub directories, the full path to the project folder must be specified."))
                                return(NULL)
                        }
                        else if(length(matches)>1){
                                #warning(paste0("Multiple StoX projects matching \"", projectName, "\". Use the full path, or path relative to the default workspace, to specify the project uniquely. The first of the following list selected:\n", paste0("\t", availableProjects$projectPaths[matches], collapse="\n")))
                                warning.length <- options("warning.length")
                                options(warning.length = 8170L)
                                stop(paste0("Multiple StoX projects matching \"", projectName, "\". Use the full path, or path relative to the default workspace (", getProjectPaths()$projectRoot, ") to specify the project uniquely:\n", paste0("\t", availableProjects$projectPaths[matches], collapse="\n")))
                                options(warning.length = warning.length)
                        }       
                        projectName <- availableProjects$projectPaths[matches[1]]
                }       
                

                projectPaths <- getProjectPaths(projectName)
                projectName <- projectPaths$projectName
                projectRoot <- projectPaths$projectRoot
                projectPath <- projectPaths$projectPath
                ########## Open the project in Java memory: ##########
                Rstox.init()
                project <- J("no.imr.stox.factory.FactoryUtil")$openProject(projectRoot, projectName)
                # This line was added on 2017-08-23 due to a bug discovered when estimating the area of polygons using the "accurate" method. When baseline is run, Java calls the function polyArea() when AreaMethod="accurate". However, whenever this failed, the simple method implemented in Java was used (which was a bug). The problem was that the path to the R-bin was not set in Rstox. To solve this, a file holding this path (or the command project$setRFolder(PATH_TO_R_BIN)) will be saved by StoX, and called every time a triggerscript is run. In Rstox we solve the problem by the below sequence:
                RFolder <- project$getRFolder()
                #__# RFolder <- rJava::.jcall(project, "S", "getRFolder")
                if(length(RFolder)==0 || (is.character(RFolder) && nchar(RFolder)==0)){
                        project$setRFolder(R.home("bin"))
                        #__# RFolder <- rJava::.jcall(project, "S", "setRFolder", R.home("bin"))
                }
                ############################################### ######

                ########## Open the project in R memory: ##########
                # Create a list for the project in the environment 'RstoxEnv':
                parameters <- readBaselineParametersJava(project)
                # The first line (temp <- getRstoxEnv()) is needed to assure that the RstoxEnv environment is loaded:
                temp <- getRstoxEnv()
                RstoxEnv$Projects[[projectName]] <- list(originalParameters=parameters, savedParameters=parameters, javaParameters=parameters, lastParameters=parameters, projectObject=project, projectPath=projectPath, projectData=new.env())
                #assign(projectName, list(originalParameters=parameters, javaParameters=parameters, lastParameters=parameters, projectObject=project, projectData=new.env()), envir=getRstoxEnv())
                ##assign(getRstoxEnv()[[projectName]], list(originalParameters=parameters, javaParameters=parameters, lastParameters=parameters, projectObject=project, projectData=new.env()))
                ###getRstoxEnv()[[projectName]] <- list(originalParameters=parameters, javaParameters=parameters, lastParameters=parameters, projectObject=project, projectData=new.env())
                # Also add the last used parameters to the projectData, in order to save this to file.
                ##### NOTE (2017-08-28): This is different from the 'lastParameters' object in the project environment (rstoxEnv[[projectName]]), and is only used when writing project data such as bootstrap results to file: #####
                setProjectData(projectName=projectName, var=parameters, name="lastParameters")

                # As of version 1.4.2, create the new folder structure:
                suppressWarnings(dir.create(projectPaths$RDataDir, recursive=TRUE))
                suppressWarnings(dir.create(projectPaths$RReportDir, recursive=TRUE))
                ###################################################
                #}
        }
        ### # Return a baseline object:
        ### if(tolower(substr(out[1], 1, 1)) == "b"){
        ###     return(project$getBaseline())
        ### }
        ### # Return the project object:
        ### else if(tolower(substr(out[1], 1, 1)) == "p"){
        ###     return(project)
        ### }
        ### # Return the project name:
        ### else if(tolower(substr(out[1], 1, 1)) == "n"){
        ###     return(project$getProjectName())
        ### }

        ### # Return a baseline object:
        ### if(startsWith(tolower(out[1]), "baseline"){
        ###     return(project$getBaseline())
        ### }
        ### # Return the baseline report object:
        ### if(startsWith(tolower(out[1]), "report"){
        ###     return(project$getBaselineReport())
        ### }
        ### # Return the project object:
        ### else if(startsWith(tolower(out[1]), "project"){
        ###     return(project)
        ### }
        ### # Return the project name:
        ### else if(startsWith(tolower(out[1]), "name"){
        ###     return(project$getProjectName())
        ### }
        ### else{
        ###     warning("Invalid value of 'out'")
        ###     return(NULL)
        ### }

        # Return the requested object:
        getProject(project, out=out)
}

#' Get StoX project paths.
#'
#' Get StoX project paths.
#'
#' @export
#'
getProjectPaths <- function(projectName=NULL, projectRoot=NULL, recursive=2){
        # Declare the output to use when somthing fails:
        out <- as.list(rep(NA, 6))
        names(out) <- c("projectName", "projectRoot", "projectPath", "RDataDir", "RReportDir", "projectXML")

        ####################################
        ##### 1. projectName is empty: #####
        ####################################
        # Return the default workspace immediately if nothing is given:
        if(length(projectName)==0){
                # The functions J and .jnew and other functions in the rJava library needs initialization:
                Rstox.init()
                out$projectRoot <- rJava::.jnew("no/imr/stox/model/Project")$getRootFolder()
                return(out)
        }
        ####################################


        ###########################################################
        ##### 2. projectName is a project or baseline object: #####
        ###########################################################
        if(any(class(projectName)=="jobjRef")){
                # If a baseline object is given:
                type <- tolower(projectName$getClass()$toString())
                if(endsWith(type, "model")){
                        projectName <- projectName$getProject()
                }
                # If a StoX project object is given:
                else if(!endsWith(type, "project")){
                        warning("Invalid projectName (must be a character sting or a baseline or project object)")
                        return(out)
                }
                projectRoot <- dirname(projectName$getProjectFolder())
                projectName <- projectName$getProjectName()
        }
        else if(!is.character(projectName)){
                warning("Invalid projectName (must be a character sting or a baseline or project object)")
                return(out)
        }
        ###########################################################

        # Change added on 2017-11-24 by Arne Johannes Holmin: When a relative or full path has been applied to getProjectPaths(), which has identified the projectName and projectRoot and other values, and the output projectNAme is used further in a function, again calling getProjectPaths(), the resulting projectPath will differ from the path returned from the first call to getProjectPaths(). This is avaided by retrieving the projectPath from the project environment by the following lines:
        projectlist <- listOpenProjects()
        if(is.character(projectName) && projectName %in% projectlist$projectName){
                projectName <- projectlist$projectPath[projectName == projectlist$projectName]
        }

        # Special behavior if an empty string is given for the projectName, in which case it is replaced by a dot, making it a tool for extracting the projectRoot:
        if(nchar(projectName)[1] == 0){
                projectName <- "."
        }

        dirnameRecursive <- function(x, recursive=1){
                for(i in seq_len(recursive)){
                        x <- dirname(x)
                }
                x
        }

        ###########################################
        ##### 3. dirname(projectName) exists: #####
        ###########################################
        projectDirName <- dirnameRecursive(projectName, recursive=recursive)
        if(isTRUE(file.info(projectDirName)$isdir) && !projectDirName %in% c(".", "", "/")){
                projectRoot <- dirname(projectName)
                projectName <- basename(projectName)
        }
        ###########################################


        ####################################################################################
        ##### 4. dirname(projectName) does not exist, but dirname(projectPath) exists: #####
        ####################################################################################
        else{
                # Does the constructed projectPath exist?:
                projectPath <- file.path(projectRoot, projectName)
                projectDirName <- dirnameRecursive(projectPath, recursive=recursive)
                if(isTRUE(file.info(projectDirName)$isdir)){
                        projectRoot <- dirname(projectPath)
                        projectName <- basename(projectPath)
                }
                # If the projectRoot was not given default it:
                else if(length(projectRoot)==0){
                        # The functions J and .jnew and other functions in the rJava library needs initialization:
                        Rstox.init()
                        projectRoot <- rJava::.jnew("no/imr/stox/model/Project")$getRootFolder()
                        projectPath <- file.path(projectRoot, projectName)
                        projectDirName <- dirnameRecursive(projectPath, recursive=recursive)
                        if(isTRUE(file.info(projectDirName)$isdir)){
                                projectRoot <- dirname(projectPath)
                                projectName <- basename(projectPath)
                        }
                }
                else{
                        warning(paste0("Invalid projectName (", projectName, ") or projectRoot (", projectRoot, ")"))
                        return(out)
                }
        }
        ####################################################################################


        projectPath <- file.path(projectRoot, projectName)

        RDataDir <- file.path(projectPath, "output", "r", "data")

        RReportDir <- file.path(projectPath, "output", "r", "report")

        projectXML <- file.path(projectPath, "process", "project.xml")

        inputDir <- file.path(projectPath, "input")

        outputDir <- file.path(projectPath, "output")


        return(list(projectName=projectName, projectRoot=projectRoot, projectPath=projectPath, RDataDir=RDataDir, RReportDir=RReportDir, projectXML=projectXML, inputDir=inputDir, outputDir=outputDir))
}

#' List open StoX projects.
#'
#' List open StoX projects.
#'
#' @export
#'
listOpenProjects <- function(){
        out <- data.frame(
                projectName = names(getRstoxEnv()$Projects),
                projectPath = sapply(getRstoxEnv()$Projects, function(x) x$projectPath), stringsAsFactors=FALSE)
        rownames(out) <- NULL
        out
}

#' Get StoX R-environment.
#'
#' Get StoX R-environment.
#'
#' @export
#' @keywords internal
#'
getRstoxEnv <- function(){
        # Regenerate the RstoxEnv is missing:
        if(!exists("RstoxEnv")){
                initiateRstoxEnv()
        }
        RstoxEnv
}

#' Initiate StoX R environment.
#'
#' Initiate StoX R environment.
#'
#' @export
#' @keywords internal
#'
initiateRstoxEnv <- function(){

        # Function used for extracting defaults and possible values of functions:
        getDefaultsAndPredefinedValues <- function(data){

                getDefaultsAndPredefinedValuesOne <- function(data){
                        fun_formals <- formals(data$Name)
                        # Remove the first the projectName:
                        fun_formals <- fun_formals[names(fun_formals) != "projectName"]
                        # Keep only the specified arguments:
                        fun_formals <- subset(fun_formals, names(fun_formals) %in% data$Parameters$Name)
                        # Evaluate the arguments:
                        fun_formalsEval <- lapply(fun_formals, eval)


                        data$Parameters$DataType <- sapply(fun_formalsEval, function(x) if(is.numeric(x) && x==round(x)) "Integer" else if(is.numeric(x) && x!=round(x)) "Double" else "String")

                        # Convert to string, by converting numeric and character using as.character() and all other using deparse():
                        data$Parameters$Values <- lapply(fun_formalsEval, function(x) if(is.numeric(x) || is.character(x)) as.character(x) else deparse(x))
                        # Assume that all the parameters of functions passed to StoX have length 1, and thus that if there are more than one value given as default, these are the possible values, and the first of these is the true default.
                        data$Parameters$DefaultValue <- sapply(data$Parameters$Values, function(x) head(x, 1))

                        data
                }

                lapply(data, getDefaultsAndPredefinedValuesOne)
        }

        # Create a Rstox environment in which the baseline objects of the various projects are placed. This allows for a check for previously run baseline models and avoids memory leakage:",
        assign("RstoxEnv", new.env(), envir=.GlobalEnv)

        ##### Define fundamental variables Rstox: #####

        # Default Java memory (increased this to 6 GB on 2018-10-15 since the memory is not bounded on Mac or Windows):
        # JavaMem = 2e9
        JavaMem <- 6e9

        # The folders in a StoX project:
        StoXFolders <- c("input", "output", "process")
        StoX_data_sources <- c(echosounder = "acoustic", biotic = "biotic", landing = "landing")
        StoXFoldersRecursive <- list(
                input = file.path("input", StoX_data_sources),
                output = file.path("output", outer(c("baseline", "r"), c("data", "report"), file.path)),
                process = "process"
        )
        StoXFoldersRecursiveSansOutput <- unlist(StoXFoldersRecursive[names(StoXFoldersRecursive) != "output"])

        # The following key strings are used to detect the data file type:
        #StoX_data_type_keys <- c(acoustic = "echosounder_dataset", biotic = "missions xmlns", landing = "Sluttseddel")
        StoX_data_type_keys <- c(acoustic = "nmdechosounder", biotic = "nmdbiotic", landing = "Sluttseddel")
        StoX_reading_processes <- c(acoustic = "ReadAcousticXML", biotic = "ReadBioticXML", landing = "ReadLandingXML")


        # NMD and StoX defines different data types (StoX has the more general category "acoustic"):
        NMD_data_sources <- c(acoustic = "echosounder", biotic = "biotic", landing = "landing")
        # The implemented NMD APIs for the NMD_data_sources:
        NMD_API_versions <- list(
                biotic = c(1, 2, 3),
                echosounder = 1,
                reference = c(1, 2),
                landing = NULL
        )

        # The format used by NMD for shapshot time:
        dateTimeNMDAPIFormat <- "%Y-%m-%dT%H.%M.%OSZ"

        # The current API and datasource formats:
        ver <- list(
                API = list(
                        #biotic = "2",
                        biotic = "3",
                        echosounder = "1",
                        reference = "2",
                        landing = NA
                ),
                reference = "2.0",
                #biotic = "1.4",
                biotic = "3.0",
                echosounder = NA,
                landing = NA
        )

        # Define project types:
        project_types <- c("AcousticTrawl", "SweptAreaLength", "SweptAreaTotal")

        # Define the process levels for the presicion estimate:
        processLevels <- c("bootstrap", "bootstrapImpute")

        # Define model types, i.e., the panes in StoX, added the project name:
        modelTypeJavaNames <- c("baseline", "baseline-report", "r", "r-report", "name")
        #modelTypeRstoxNames <- c("baseline", "report", NA, NA),
        modelTypeJavaFuns <- c("getBaseline", "getBaselineReport", "getRModel", "getRModelReport", "getProjectName")

        # Define ECA covariates:
        ECACovariates <- data.frame(
                Name = c(
                        "temporal",
                        "gearfactor",
                        "spatial",
                        "platformfactor"
                ),
                Processe = c(
                        "DefineTemporalLanding",
                        "DefineGearLanding",
                        "DefineSpatialLanding",
                        "DefinePlatformLanding"
                ),
                Description = c(
                        "The temporal covariate",
                        "The gear covariate given as groups of gear codes",
                        "The spatial covariate giving polygons or locations",
                        "The platform covariate (vessels)"
                ),
                stringsAsFactors = FALSE
        )


        # Define functions passed to the "R" and "R report" pane of StoX, along with associated aliases (function names that should still work for backwards compatibility):
        RFunctions <- list(
                # 1. The runBootstrap() function:
                runBootstrap = list(
                        Name = "runBootstrap",
                        Alias = c("bootstrapBioticAcoustic", "bootstrapBioticSweptArea"),
                        Description = "The Rstox function 'runBootstrap' resamples (with replacement) trawl stations and acoustic transects to obtain 'nboot' bootstrap replicates of the output from the process SuperIndAbundance, which can be used to estimate precision of the survey estimates.",
                        Parameters = list(
                                Name = c(
                                        "bootstrapMethod",
                                        "acousticMethod",
                                        "bioticMethod",
                                        "nboot",
                                        "startProcess",
                                        "endProcess",
                                        "seed",
                                        "cores"
                                ),
                                Description = c(
                                        "The method to use for the bootstrap. Currently implemented are (1) 'AcousticTrawl': Bootstrap of acoustic tralw surveys, where both acoustic and biotic data are resampled, (2) 'SweptAreaLength': Bootstrap only biotic data with length information, and (3) 'SweptAreaTotal': For surveys with information only about total catch (count or weight), bootstrap biotic stations.",
                                        "Specification of the method to use for bootstrapping. Currently only one method is available for acoustic data: acousticMethod = PSU~Stratum.",
                                        "Specification of the method to use for bootstrapping. Currently only one method is available for biotic data: bioticMethod = PSU~Stratum.",
                                        "Number of bootstrap replicates.",
                                        "The start process of the bootstrapping such as 'TotalLengthDist' (the last process before bio-stations are assigned and NASC values are calculated).",
                                        "The end process of the bootstrapping such as 'SuperIndAbundance' (the process returning a matrix with the columns 'Stratum', 'Abundance', and 'weight', as well grouping variables such as 'age', 'SpecCat', 'sex').",
                                        "The seed for the random number generator (used for reproducibility).",
                                        "An integer giving the number of cores to run the bootstrapping over."
                                )
                        )
                ),
                # 1. The runBootstrap() function:
                runBootstrap_1.6 = list(
                        Name = "runBootstrap_1.6",
                        Alias = NULL,
                        Description = "The Rstox function 'runBootstrap_1.6' is a replicate of the 'runBootstrap' in Rstox 1.6, after which the function was changed by applying sorting prior to sampling to avoid platform specific results. Using 'runBootstrap_1.6' assures identical results compared to Rstox 1.6.",
                        Parameters = list(
                                Name = c(
                                        "bootstrapMethod",
                                        "acousticMethod",
                                        "bioticMethod",
                                        "nboot",
                                        "startProcess",
                                        "endProcess",
                                        "seed",
                                        "cores"
                                ),
                                Description = c(
                                        "The method to use for the bootstrap. Currently implemented are (1) 'AcousticTrawl': Bootstrap of acoustic tralw surveys, where both acoustic and biotic data are resampled, (2) 'SweptAreaLength': Bootstrap only biotic data with length information, and (3) 'SweptAreaTotal': For surveys with information only about total catch (count or weight), bootstrap biotic stations.",
                                        "Specification of the method to use for bootstrapping. Currently only one method is available for acoustic data: acousticMethod = PSU~Stratum.",
                                        "Specification of the method to use for bootstrapping. Currently only one method is available for biotic data: bioticMethod = PSU~Stratum.",
                                        "Number of bootstrap replicates.",
                                        "The start process of the bootstrapping such as 'TotalLengthDist' (the last process before bio-stations are assigned and NASC values are calculated).",
                                        "The end process of the bootstrapping such as 'SuperIndAbundance' (the process returning a matrix with the columns 'Stratum', 'Abundance', and 'weight', as well grouping variables such as 'age', 'SpecCat', 'sex').",
                                        "The seed for the random number generator (used for reproducibility).",
                                        "An integer giving the number of cores to run the bootstrapping over."
                                )
                        )
                ),
                # 1. The runBootstrap() function:
                imputeByAge = list(
                        Name = "imputeByAge",
                        Alias = NULL,
                        Description = "The Rstox function 'imputeByAge' imputes missing data in the output from 'runBootstrap' at rows with missing age. The imputation draws rows with non-missing age within the same length group as the row with missing data, searching for these matches first inside the station, then inside the stratum, and finally inside the survey.",
                        Parameters = list(
                                Name = c(
                                        "seed",
                                        "cores"
                                ),
                                Description = c(
                                        "The seed for the random number generator (used for reproducibility).",
                                        "An integer giving the number of cores to run in parallel."
                                )
                        )
                ),
                # 1. The runBootstrap() function:
                saveProjectData = list(
                        Name = "saveProjectData",
                        Alias = "saveRImage",
                        Description = "The Rstox function 'saveProjectData' saves data such as bootstrap replicates to RData-files in the output/r/data directory.",
                        Parameters = list(
                                Name = c(),
                                Description = c()
                        )
                )
        )
        RFunctions <- getDefaultsAndPredefinedValues(RFunctions)

        # Define functions passed to the "R" and "R report" pane of StoX, along with associated aliases (function names that should still work for backwards compatibility):
        RReportFunctions <- list(
                # 1. The runBootstrap() function:
                getReports = list(
                        Name = "getReports",
                        Alias = NULL,
                        Description = "The Rstox function 'getReports' runs the report functions specified by 'out'.",
                        Parameters = list(
                                Name = c(
                                        "out",
                                        "options"
                                ),
                                Description = c(
                                        "The report functions to run. Keyword 'all' implies to run all relevant report functions",
                                        "Options given to the report functions given as R expressions separated by semicolons or commas in cases where no commas are used in the expressions. Example: \"grp1 = 'age'; grp2 = 'sex'\" or \"grp1 = NULL\" for returning TSN and \"grp1 = NULL; var='weight'\" for returning TSB."
                                )
                        )
                ),
                # 1. The runBootstrap() function:
                getPlots = list(
                        Name = "getPlots",
                        Alias = NULL,
                        Description = "The Rstox function 'getPlots' runs the plot functions specified by 'out'.",
                        Parameters = list(
                                Name = c(
                                        "out",
                                        "options"
                                ),
                                Description = c(
                                        "The plotting functions to run. Keyword 'all' implies to run all relevant plotting functions",
                                        "Options given to the plotting functions given as R expressions separated by semicolons or commas in cases where no commas are used in the expressions. Example: \"grp1 = 'age'; grp2 = 'sex'\" or \"grp1 = NULL\" for plotting TSN and \"grp1 = NULL; var='weight'\" for plotting TSB."
                                )
                        )
                )
        )
        RReportFunctions <- getDefaultsAndPredefinedValues(RReportFunctions)

        # Assign to RstoxEnv and return the definitions:
        Definitions <- list(
                JavaMem = JavaMem,
                StoXFolders = StoXFolders,
                StoXFoldersRecursive = StoXFoldersRecursive,
                StoXFoldersRecursiveSansOutput = StoXFoldersRecursiveSansOutput,
                StoX_data_sources = StoX_data_sources,
                StoX_data_type_keys = StoX_data_type_keys,
                StoX_reading_processes = StoX_reading_processes,
                dateTimeNMDAPIFormat = dateTimeNMDAPIFormat,
                NMD_data_sources = NMD_data_sources,
                #NMD_API_versions = NMD_API_versions,
                ver = ver,
                project_types = project_types,
                processLevels = processLevels,
                modelTypeJavaNames = modelTypeJavaNames,
                modelTypeJavaFuns = modelTypeJavaFuns,
                ECACovariates = ECACovariates,
                RFunctions = RFunctions,
                RReportFunctions = RReportFunctions
        )
        assign("Definitions", Definitions, envir=get("RstoxEnv"))
        assign("Projects", list(), envir=get("RstoxEnv"))
        return(Definitions)
}

#' Run StoX bootstrap.
#'
#' Run StoX bootstrap.
#'
#' @export
#'
runBootstrap <- function(projectName, bootstrapMethod="AcousticTrawl", acousticMethod=PSU~Stratum, bioticMethod=PSU~Stratum, nboot=5, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seed=1, cores=1, msg=TRUE, sorted=TRUE, JavaMem=getRstoxDef("JavaMem"), ...){
        lll <- list(...)
        ### Backwards compatibility: ###
        # If the old numIterations is given, override the nboot by this:
        if(length(lll$numIterations)){
                nboot <- lll$numIterations
        }
        ### End of backwards compatibility: ###

        # Open the project:
        openProject(projectName)
        # Get the baseline parameters, which will be used to reset the project after the bootstrapping, since ... may contain changes in parameters:
        parameters <- getBaselineParameters(projectName)$java

        # Run the different bootstrap types:
        temp <- getBootstrapMethod(bootstrapMethod=bootstrapMethod, acousticMethod=acousticMethod, bioticMethod=bioticMethod, ...)
        bootstrapMethod <- temp$bootstrapMethod
        acousticMethod <- temp$acousticMethod
        bioticMethod <- temp$bioticMethod

        # Apply the bootstrap:
        if(!bootstrapMethod %in% getRstoxDef("project_types")){
                stop("Invalid bootstrap type.")
        }
        bootstrapFun <- paste("runBootstrap", bootstrapMethod, sep="_")
        do.call(bootstrapFun, list(projectName=projectName, acousticMethod=acousticMethod, bioticMethod=bioticMethod, nboot=nboot, startProcess=startProcess, endProcess=endProcess, seed=seed, cores=cores, msg=msg, sorted=sorted, JavaMem=JavaMem, ...))

        # Reset to the last baseline run before the bootstrap:
        runBaseline(projectName, parlist=parameters, out="name", reset=TRUE, msg=FALSE)
}

#' Run StoX bootstrap, version 1.6.
#'
#' Run StoX bootstrap, version 1.6.
#'
#' @export
#'
runBootstrap_1.6 <- function(projectName, bootstrapMethod="AcousticTrawl", acousticMethod=PSU~Stratum, bioticMethod=PSU~Stratum, nboot=5, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seed=1, cores=1, msg=TRUE, ...){
        g <- getPrecisionLevel(projectName)
        setPrecisionLevel(projectName, 0L)
        runBootstrap(projectName=projectName, bootstrapMethod=bootstrapMethod, acousticMethod=acousticMethod, bioticMethod=bioticMethod, nboot=nboot, startProcess=startProcess, endProcess=endProcess, seed=seed, cores=cores, msg=msg, sorted=FALSE, ...)
        setPrecisionLevel(projectName, g)
}

#' Impute by age.
#'
#' Impute by age.
#'
#' @export
#'
imputeByAge <- function(projectName, seed=1, cores=1, saveInd=TRUE, ...){

        # Write a dot at each iteration to a textfile:
        #dotfile <- file.path(getProjectPaths(projectName)$RReportDir, "imputeProgress.txt")
        #write("", dotfile)

        # Read the saved data from the R model. In older versions the user loaded the file "rmodel.RData" separately, but in the current code the environment "RstoxEnv" is declared on load of Rstox, and all relevant outputs are assigned to this environment:
        imputeVariable <- getProjectData(projectName=projectName, var="bootstrap")

        nboot <- length(imputeVariable$SuperIndAbundance) ## The length of the data collection corresponds to the number of bootstrap iterations
        imputeSummary.out <- vector("list", nboot)
        indMissing.out <- vector("list", nboot)
        indReplacement.out <- vector("list", nboot)
        seedM.out <- vector("list", nboot)

        # Set the seed of the runs, either as a vector of 1234s, to comply with old code, where the seeed was allways 1234 (from before 2016), or as a vector of seeds sampled with the given seed, or as NULL, in which case the seed matrix 'seedM' of distributeAbundance() is set by sampling seq_len(10000000) without seed:
        # Change introduced on 2017-11-14 by Holmin. To make the code more robust to changes, all generation of seeds has been moved to the functions setSeedSingle(), getSeedV(), getSeedM(), expandSeed():
        #if(isTRUE(seed)){
        #       seedV = rep(TRUE, nboot+1) # seed==TRUE giving 1234 for compatibility with older versions
        #}
        #else if(is.numeric(seed)){
        #       set.seed(seed)
        #       seedV = sample(seq_len(10000000), nboot+1, replace=FALSE)
        #}
        #else{
        #       seedV = NULL
        #}
        seedV <- expandSeed(seed, nboot)



        # Store the bootstrap iteration names:
        namesOfIterations <- names(imputeVariable$SuperIndAbundance)

        # Impute biological information
        imputeVariable$base.SuperIndAbundance <- distributeAbundance(i=1, abnd=imputeVariable$base.SuperIndAbundance, seedV=tail(seedV,1))$data

        # Check available cores:
        availableCores = detectCores()
        # If memory runs out, a system call to determine number of cores might fail, thus detectCores() could return NA
        # defaulting to single core if this is the case
        if(is.na(availableCores)) availableCores <- 1
        if(cores>availableCores){
                warning(paste0("Only ", availableCores, " cores available (", cores, " requested)"))
        }
        cores = min(cores, nboot, availableCores)
        # Generate the clusters of time steps:

        if(cores>1){
                cat(paste0("Imputing missing data (", nboot, " replicates, using ", cores, " cores in parallel):\n"))
                cl<-makeCluster(cores)
                # Bootstrap:
                out <- pblapply(seq_len(nboot), distributeAbundance, abnd=imputeVariable$SuperIndAbundance, seedV=seedV, cl=cl)
                # End the parallel bootstrapping:
                stopCluster(cl)
        }
        else{
                cat(paste0("Imputing missing data (", nboot, " replicates):\n"))
                out <- pblapply(seq_len(nboot), distributeAbundance, abnd=imputeVariable$SuperIndAbundance, seedV=seedV)
        }

        imputeVariable$SuperIndAbundance <- lapply(out, "[[", "data")
        # Add names ot the iterations:
        names(imputeVariable$SuperIndAbundance) <- namesOfIterations
        imputeSummary.out <- lapply(out, "[[", "imputeSummary")
        indMissing.out <- lapply(out, "[[", "indMissing")
        indReplacement.out <- lapply(out, "[[", "indReplacement")
        seedM.out <- lapply(out, "[[", "seedM")
        #
        #
        #names(imputeVariable$SuperIndAbundance) <- namesOfIterations
        #msg.out <- lapply(out, "[[", "msg")
        #indMissing.out <- lapply(out, "[[", "indMissing")
        #indReplacement.out <- lapply(out, "[[", "indReplacement")
        #seedM.out <- lapply(out, "[[", "seedM")

        # imputeSummary.out <- t(as.data.frame(imputeSummary.out))
        # Using do.call("rbind", imputeSummary.out) did not result in a data frame on which the $ operator works. Instead we use data.table::rbindlist, but convert back to data frame:
        #imputeSummary.out <- do.call("rbind", imputeSummary.out)
        imputeSummary.out <- as.data.frame(data.table::rbindlist(imputeSummary.out))
        #colnames(msg.out) <- c("Aged", "NotAged", "ImputedAtStation", "ImputedAtStrata", "ImputedAtSurvey", "NotImputed")
        #colnames(imputeSummary.out) <- c("NumAged", "NumNotAged", "NumUsed", "NumNoMatch", "NumImputedAtStation", "NumImputedAtStratum", "NumImputedAtSurvey")
        #rownames(imputeSummary.out) <- paste0("Iter", seq_len(nboot))
        rownames(imputeSummary.out) <- names(imputeVariable$SuperIndAbundance)

        # Issue warnings for runs with no unknown, and no known ages:
        NatKnownAge0 <- which(imputeSummary.out$NatKnownAge==0)
        #NumUsedNA <- which(is.na(imputeSummary.out$NumUsed))
        if(length(NatKnownAge0)){
                warning("The following bootstrap runs had no known ages, resulting in no imputing: ", paste(NatKnownAge0, collapse=", "))
        }
        #if(length(NumUsedNA)){
        #       warning("The following bootstrap runs had no unknown ages, resulting in no imputing: ", paste(NumUsedNA, collapse=", "))
        #}


        # Store the output messages, the missing and replace indices, the seeds and other parameters of the imputing:
        imputeVariable$imputeParameters$imputeSummary <- imputeSummary.out
        imputeVariable$imputeParameters$seed <- seed
        imputeVariable$imputeParameters$seedV <- seedV
        imputeVariable$imputeParameters$seedM <- seedM.out
        imputeVariable$imputeParameters$nboot <- nboot
        imputeVariable$imputeParameters$cores <- cores
        # Add bootstrap methods:
        imputeVariable$imputeParameters$bootstrapMethod <- imputeVariable$bootstrapParameters$bootstrapMethod
        imputeVariable$imputeParameters$acousticMethod  <- imputeVariable$bootstrapParameters$acousticMethod
        imputeVariable$imputeParameters$bioticMethod    <- imputeVariable$bootstrapParameters$bioticMethod

        if(saveInd){
                imputeVariable$imputeParameters$indMissing <- indMissing.out
                imputeVariable$imputeParameters$indReplacement <- indReplacement.out
        }

        # Assign the data to the environment of the project:
        setProjectData(projectName=projectName, var=imputeVariable, name="bootstrapImpute")

        return(imputeSummary.out)
}

#' Save StoX project data.
#'
#' Save StoX project data.
#'
#' @export
#'
saveProjectData <- function(projectName, var="all", ...){
        projectDataEnv <- getProjectDataEnv(projectName)
        if(length(projectDataEnv)==0){
                warning("Project not open")
                return(FALSE)
        }
        if(identical(tolower(var), "all")){
                var <- ls(projectDataEnv)
        }
        else{
                var <- intersect(var, ls(projectDataEnv))
        }
        if(length(var)==0){
                warning(paste0("'var' not matching any of the available data objects (", paste(ls(projectDataEnv), collapse=", "), "). No data saved"))
                return(FALSE)
        }

        # Get the project RData directory and the trash sub directory:
        projectPaths <- getProjectPaths(projectName)
        trashDir <- file.path(projectPaths$RDataDir, "trash")
        #Empty trash, but only those:
        #unlink(trashDir, recursive=TRUE, force=TRUE)

        # Move files to the trash:
        suppressWarnings(dir.create(trashDir, recursive=TRUE))
        files <- file.path(projectPaths$RDataDir, paste0(var, ".RData"))
        existingFiles <- file.exists(files)
        file.copy(files[existingFiles], trashDir, overwrite=TRUE, recursive=TRUE)
        unlink(files, recursive=TRUE, force=TRUE)

        # Save files:
        #lapply(var, function(x) save(list=x, file=file.path(projectPaths$RDataDir, paste0(x, ".RData")), envir=projectDataEnv))
        lapply(seq_along(var), function(i) save(list=var[i], file=files[i], envir=projectDataEnv))
        invisible(files)
}

#' Get StoX reports.
#'
#' Get StoX reports.
#'
#' @export
#'
getReports <- function(projectName, out="all", options="", ...){
        runFunsRstox(projectName, string="report", out=out, options=options, write=TRUE, all.out=TRUE, ...)
}

#' Get StoX plots
#'
#' Get StoX plots
#'
#' @param projectName Stox project name.
#' @param out Stox output container.
#' @param options Stox options
#' @param ... Any other params to be passed on.
#'
#' @export
#'
getPlots <- function(projectName, out="all", options="", ...){
        runFunsRstox(projectName, string="plot", out=out, options=options, ...)
}

#' Set single seed.
#'
#' Set single seed.
#'
#' @export
#' @keywords internal
#'
setSeedSingle <- function(seed){
        set.seed(if(isTRUE(seed)) 1234 else if(is.numeric(seed)) seed else NULL) # seed==TRUE giving 1234 for compatibility with older versions
}

#' Gets vector of fixed seeds for a given seed
#'
#' Gets vector of fixed seeds for a given seed
#'
#' @export
#' @keywords internal
#'
getSeedV <- function(seed, nboot){
        setSeedSingle(seed)
        SeedV <- sample(getSequenceToSampleFrom(), nboot, replace=FALSE) # Makes seed vector for fixed seeds (for reproducibility).
        SeedV
}

#' Get 10 million to sample from.
#'
#' Get 10 million to sample from.
#'
#' @export
#' @keywords internal
#'
getSequenceToSampleFrom <- function(){
        size <- 1e7
        seq_len(size)
}

#' Get log of Stox ID.
#'
#' Get log of Stox ID.
#'
#' @export
#' @keywords internal
#'
getLogStoXid <- function(Log, timevar="start_time"){
        dateSlashTime <- gsub(" ", "/", as.character(Log[[timevar]]), fixed=TRUE)
        log_start <- trimws(format(Log$log_start, nsmall=1))
        Log$logStoXid <- paste(Log$cruise, log_start, dateSlashTime, sep="/")
        Log
}

#' Get StoX process data table as dataframe.
#' 
#' Get StoX process data table as dataframe.
#' 
#' @export
#' @keywords internal
#' 
getProcessDataTableAsDataFrame <- function(projectName, tableName) {
        # Get the project object:
        project <- openProject(projectName, out="project")
        s <- project$getProcessData()$asTable(tableName)
        if(nchar(s)>0){
                out <- read.csv(textConnection(s), sep='\t', row.names=NULL, stringsAsFactors=FALSE, na.strings="-", encoding="UTF-8")
                # Interpret true/false as TRUE/FALSE:
                for(i in seq_along(out)){
                        #if(length(out[[i]])>0 && head(out[[i]], 1) %in% c("true", "false")){
                        if(length(out[[i]])>0 && all(out[[i]] %in% c("true", "false"))){
                                out[[i]] <- as.logical(out[[i]])
                        }
                }
                return(out)
        }
        else{
                warning(paste0("Table \"", tableName, "\" missing in the project.xml file"))
                return(NULL)
        }
}

