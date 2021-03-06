#' @title dropbox.data
#'
#' @description This file is used to load dropbox package data into the the global environment. It also identifies and returns all dropbox paths for the current package
#' @param pkgname The package name
#' @param Sys_dropbox_dir The location of the root dropbox folder Default: ~/Dropbox
#' @keywords corelogic import
#' @export
#' @import rdrop2
#'     stringr
#'     data.table
data.env <- function(pkgname, pkgurl) {
    if (length(Sys_dropbox_dir)==0){
        l.dirs <- list()
        l.dirs[[1]] <- ('/')
        while(i < 10 & length(Sys_dropbox_dir)==0){
            l.dirs[[i]] <- unlist(lapply(l.dirs[[i-1]], function(x) list.dirs(path = x, full.names = TRUE, recursive = FALSE)))
            Sys_dropbox_dir <- l.dirs[[i]][which(grepl('\\/Dropbox$', l.dirs[[i]]))]
        }
        if(length(Sys_dropbox_dir)==0){
            stop('Dropbox directory not found. Please install dropbox on computer.')
        }
    }
    # Find dropbox directory
    rdrop2::drop_auth()
    cat('Mapping data to package', pkgname, 'dropbox location', '\n')
    drop_user <- rdrop2::drop_acc()[[2]]
    cat(paste('\nUser', drop_user, 'authenticated in dropbox'), sep='\n')
    if (length(drop_user)==0){
        cat('You must have Dropbox installed on local computer to use', pkgname)
        stop()
    }
    if (dir.exists(Sys_dropbox_dir)){
        #cat('Dropbox system directory found at', Sys_dropbox_dir,'\n')
    } else {
        cat(Sys_dropbox_dir, 'Missing, easy searching easy for Dropbox directory in home directory.')
        dirs <- list.dirs(path = "~/", full.names = TRUE, recursive = FALSE)
        drop.raw <- grep('\\/Dropbox(?=$)', dirs, value=TRUE, perl=TRUE, ignore.case = FALSE)
        if (length(drop.raw)>0){
            Sys_dropbox_dir <- gsub('(/){2,}', '/', drop.raw, perl=TRUE)
            cat('Setting dropbox system directory to', Sys_dropbox_dir, '\n')
        } else {
            'Searching deep for Dropbox directory '
            dirs <- list.dirs(path = "~/", full.names = TRUE, recursive = TRUE)
            drop.raw <- grep('\\/Dropbox(?=$)', dirs, value=TRUE, perl=TRUE, ignore.case = FALSE)
            if (length(drop.raw)>0){
                Sys_dropbox_dir <- gsub('(/){2,}', '/', drop.raw, perl=TRUE)
                cat('Setting dropbox system directory to', Sys_dropbox_dir,'\n')
            }
        }
    }
    R_dropbox_dir <- paste0('/pkg.data/', pkgname)
    R_dropbox_file <- paste0(R_dropbox_dir, '/', pkgname, '.rdata')
    Sys_package_dir <- paste0(Sys_dropbox_dir, 'pkg.data/', pkgname)
    Sys_package_file <- paste0(Sys_package_dir, '/', pkgname, '.rdata')
    Sys_pkgdata_dir <- paste0(Sys_dropbox_dir,'pkg.data/')

    if (!dir.exists(Sys_pkgdata_dir)){
        cat(paste('Creating pkg.data repository', Sys_pkgdata_dir), sep='\n')
        dir.create(paste0(Sys_pkgdata_dir,'/'))
    }
    if (!dir.exists(Sys_package_dir)){
        cat(paste('Linking package repository',  Sys_package_dir), sep='\n')
        dir.create(Sys_package_dir)
    }
    # View which files are uploaded
    cat(paste('Directory', Sys_package_dir, 'details:'), sep='\n')
    drop.files.dirs <- list.dirs(path = Sys_package_dir, full.names = TRUE, recursive = TRUE)
    drop.files.dirs <- sapply(drop.files.dirs, function(x) str_extract(x, regex('/pkg.data/.+.', perl=TRUE)))
    # Convert all dirs to lower case and replace spaces with_(weird issue)
    drop.files$all <-  rbindlist(lapply(drop.files.dirs, function(x) data.table(as.data.table(rdrop2::drop_dir(x)), orig.dir=x)))
    if (nrow(drop.files$all)>0){
        drop.files$all$f.name <- str_extract(drop.files$all$path, regex('[^/]+$', perl=TRUE))
        # Double check validity because of capitalization bug in rdrop2
        #drop.files$all$new.dir <- str_replace_all(drop.files$all$path, paste0('/',drop.files$all$f.name), '')
        #drop.files$all$bad.dir <- drop.files$all$new.dir != drop.files$all$orig.dir
        # If there is an issue,
        drop.files$all$sys_path_file <- paste0(Sys_dropbox_dir, gsub('/pkg.data', 'pkg.data', drop.files$all$path))
        drop.files$base <- drop.files$all[which(drop.files$all$path %in% R_dropbox_file),]
        drop.files$raw <- drop.files$all[which(!drop.files$all$path %in% R_dropbox_file),]
        drop.files$sys_path <- Sys_dropbox_dir

        # Check for the base data such as (corelogic.rdata)
        if (nrow(drop.files$all)==0){
            cat('---------------------------------------------------------',
                paste0('Error in load ', pkgname,'.rdata : No raw or final data available.'),
                paste('----------------------------------------------------------'),
                paste('Too fix this issue try the following:'),
                paste('1) Make sure the dropbox directory ', R_dropbox_dir, 'is synced on the local computer'),
                paste('2) If you do not have adequate sharing permissions through dropbox, contact erikbjohn@gmail.com for dropbox sharing.'),
                paste('   Subject line should read: ', R_dropbox_dir, 'package data sharing for user email:',  rdrop2::drop_acc()[[15]]), sep='\n')
        }
        if (nrow(drop.files$base)>0){
            cat('\n', paste(R_dropbox_file, 'exists.'),
                paste0('Loading ', R_dropbox_file, 'to Global envirornment'),
                paste('Last modified:', drop.files$base$modified),
                paste0('To create new ', pkgname, '.rdata, call the function: ', pkgname,'::', pkgname, '(new=TRUE)'),
                sep='\n')
            load(Sys_package_file, envir = .GlobalEnv)
        } else {
            cat(paste0(R_dropbox_file, ' does not exist, please run ', pkgname, '() to create'), sep='\n')
        }
    } else {
        cat(Sys_package_dir, 'is empty.')
    }
    return(drop.files)
}
