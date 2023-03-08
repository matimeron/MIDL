Pro Spec_file_join, first = fnam, second = snam, result = rnam, verbose = vrb

;+
; NAME:
;		SPEC_FILE_JOIN
; VERSION:
;		5.6
; PURPOSE:
;		Joins two spec files together.
; CATEGORY:
;		SPEC file processing
; CALLING SEQUENCE:
;		Result = SPEC_FILE_JOIN ([keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FIRST
;		Name of the first file to be joined.  Optional, quereed for
;		interactively if not provided.
;	SECOND
;		Name of the second file to be joined.  Optional, quereed for
;		interactively if not provided.
;	RESULT
;		Name for the joined file to be written.  If not provided, a tentative
;		name is generated from a combintation of the names of the source files,
;		with the user having the option to overwrite it, interactively.
;	/VERBOSE
;		Switch.  When set, the full file name of the result file is written to
;		the screen.
; OUTPUTS:
;		None other then the result file which is written to the disk.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Combines both files, where the first file is taken as is while the
;		second file has its header dropped and its scan numbers adjusted to be
;		a continuation of the scan numbers of the first file.  Calls DEFAULT,
;		FILE_GET, FNAMPARSE, RASCLINE, STREQ and STRPARSE_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-OCT-2006 by Mati Meron.
;-

	on_error, 1

	fptarr = Rascline(fnam, lin= farr, file_name= wfnam, call= 2, stat= stat)
	if stat then farr = strtrim(farr,1) else message, 'Missing first file!'
	sptarr = Rascline(smam, lin= sarr, file_name= wsnam, call= 2, stat= stat)
	if stat then sarr = strtrim(sarr,1) else message, 'Missing second file!'

	fsc = where(Streq(farr,'#S',2),fsn)
	if fsn gt 0 then begin
		dum = Strparse_mm(farr[fsc[fsn-1]],' 	',lis)
		off = long(lis[1])
	endif else off = 0l

	ssc = where(Streq(sarr,'#S',2),ssn)
	for i = 0, ssn-1 do begin
		dum = Strparse_mm(sarr[ssc[i]],' 	',lis)
		lis[1] = strtrim(string(long(lis[1]) + off),2)
		sarr[ssc[i]] = strjoin(lis,' ')
	endfor
	if ssn gt 0 then sarr = sarr[ssc[0]:*]

	dum = where(strlen(farr) gt 0,ndum)
	if ndum gt 0 then farr = farr[0:dum[ndum-1]]
	rarr = [farr,'',sarr]
	tnam = Default(rnam,Fnamparse(wfnam) + '_' + Fnamparse(wsnam),/dtyp)
	wnam = File_get(tnam,def='dat',/write,/over,stat=stat)
	if stat then begin
		openw, unit, wnam, /get_lun
		printf, unit, rarr, format = '(a)'
		free_lun, unit
		if keyword_set(vrb) then begin
			print
			print, '	Saved ' + wnam
			print
		endif
	endif else message, 'Name error, file not written!', /cont

	return
end