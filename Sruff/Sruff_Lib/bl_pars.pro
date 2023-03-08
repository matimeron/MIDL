Pro BL_pars, filename, synch = syn, show = sho, reload = rel

;+
; NAME:
;		BL_PARS
; VERSION:
;		8.716
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
;			APS			-	APS, current lattice (this is the default).
;			APSU_0		-	APS MBA lattice, coupling = 1, preliminary.
;			APSU_Timing	-	APS MBA lattice, coupling = 1.
;			APSU_Bright	-	APS MBA lattice, coupling = 0.1.
;			NSLS2_LB	-	NSLS2, low beta section.
;			NSLS2_HB	-	NSLS2, high beta section.
;			TPS			-	TPS, high beta section (??).
;			ESRF		-	ESRF, high beta section.
;			SPring-8	-	SPring-8, long ID section.
;			PETRA III	-	PETRA III undulator beamline.
;			DIAMOND		-	DIAMOND undulator beamline.
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
;		Completely rewritten 20-OCT-2013 by Mati Meron.  Internal changes,
;		transparent to the user.
;		Modified 1-SEP-2016 by Mati Meron.
;		Modified 20-DEC-2019 by Mati Meron.  Changes in comments block. 
;-

	common blpar_stuff, nfl, sortab
	on_error, 1

	syfl = Type(syn) ne 0
	refl = keyword_set(rel)
	defsysv, '!blpar', exists = exfl

	if refl then nfl = 0
	if Default(nfl,0) eq 0 then begin
		fname = getenv('sruff_data') + 'sync_sources'
		filename  = Default(filename,fname + '.sav')
		restore, filename
		nfl = 1
	endif

	if (not exfl) or syfl or refl then begin
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
		endif else begin
			if exfl then ind = Strmatch_mm(!blpar.sync,posib) else ind = 0
		endelse
		defsysv, '!blpar', sortab[ind]
	endif

	if keyword_set(sho) then help, /st, !blpar

	return
end