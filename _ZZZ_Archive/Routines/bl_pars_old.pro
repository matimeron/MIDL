Pro BL_pars_old, filename, synch = syn, show = sho, reload = rel

;+
; NAME:
;		BL_PARS
; VERSION:
;		8.213
; PURPOSE:
;		Creates or updates a system variable named !BLPAR.  !BLPAR is a
;		structure the fields of which contain values of constants used in 
;		machine specific synchrotron calculations.  See the SOURCE__DEFINE 
;		routine for details.
; CATEGORY:
;		Utility.
; CALLING SEQUENCE:
;		BL_PARS [, FILENAME] [, keywords]
; INPUTS:
;	FILENAME
;		The name of the XDR file containing the synchrotron source data.  
;		Default is sruff_data\sync_sources.sav where sruff_data is an
;		environment variable pointing to the \sruff\data subdirectory in MIDL, 
;		thus FILENAME is only needed if different initialization file is to be
;		used.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS
; 	SYNCH
; 		Name of Synchrotron.  Currently accepted names are"
; 			APS			-	This is the default
; 			ASPU_0		-	APS MBA lattice, preliminary
; 			NSLS2_LB	-	NSLS-II low beta beamline.
; 			NSLS2_HB	-	NSLS-II high beta beamline.
; 		Note:	If any of the names above is entered the corresponding data will
; 				be loaded in !BLPAR.  If any other name (including null string)
; 				is entered, the routine will query for a name, interactively.
;	/SHOW
;		Switch.  If set, basic information about the structure !BLPAR is shown.
;		Note that /SHOW is executed after SYNCH so that, if the data was changed
;		through SYNCH, it is the newest data that will be shown.
;	/RELOAD
;		Switch.  If set, the synchrotron sources data file is reloaded.  To be
;		used following updates of the data file.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		BLPAR_STUFF.  Contains
;			NFL		-	Flag, set to 1 after the data has been loaded.
;			SORTAB	-	An array of structures, containing sources' data.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Either creates or, if already exists, updates the system variable
;		!BLPAR.  Calls DEFAULT, STRMATCH_MM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUN-2005 by Mati Meron.
;		Modified 5-FEB-2006 by Mati Meron.  Added fields RSIG and ASIG.
;		Modified 30-DEC-2006 by Mati Meron.  Added field MGAP.
;		Modified 25-APR-2013 by Mati Meron.  Added options NSLS2_LB and NSLS2_HB
;		Modified 20-SEP-2013 by Mati Meron.  Added option APS_MBA_0.
;		Completely rewritten 20-OCT-2013.  Internal changes, transparent to the
;		user.
;-

	common blpar_stuff, nfl, sortab
	on_error, 1

	if keyword_set(rel) then nfl = 0
	if Default(nfl,0) eq 0 then begin
		fname = getenv('sruff_data') + 'sync_sources'
		filename  = Default(filename,fname + '.sav')
		restore, filename
		nfl = 1
	endif

	defsysv, '!blpar', exists = exfl
	shfl = keyword_set(sho)
	syfl = Type(syn) ne 0

	if not (exfl and shfl) or syfl then begin
		posib = sortab.(0)
		npos = n_elements(posib)
		if syfl then begin
			ind = Strmatch_mm(syn,posib)
			if ind eq -1 then begin
				descs = sortab.(1)
				print
				print, 'Available sources are:
				print
				for i = 0, npos - 1 do $
				print, i, posib[i], descs[i], form = '(i0,")	",a," - ",a)'
				print
				read, ind, prompt = 'Which one (enter #)? '
			endif
			if ind eq -1 or ind ge npos then message, 'Unknown source!'
		endif else ind = 0
		wsyn = posib[ind]
	endif

	if exfl then if syfl then make = (!blpar.sync ne wsyn) else make = 0 $
	else make = 1
	if make then defsysv, '!blpar', sortab[ind]
	if shfl then help, /st, !blpar

	return
end