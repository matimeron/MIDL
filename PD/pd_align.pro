Pro PD_align, pick = pic, show = sho

;+
; NAME:
;		PD_ALIGN
; VERSION:
;		7.15
; PURPOSE:
;		Reads a Pilatus alignment scan, and finds the "center channel",
;		corresponding to the direct beam.
; CATEGORY:
;		Surface specific
; CALLING SEQUENCE:
;		PD_ALIGN [, /PICK] [, /SHOW]
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/PICK
;		Switch, if set the alignment frame selection is manual.  By default the
;		selection is automatic, unless the is no defined "current SPEC file".
;	/SHOW
;		Switch, if set the alignment file is displayed to the screen.
; OUTPUTS:
;		None other than the screen output of center location.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		If no current SPEC file is defined, asks the user to locate an
;		alignment file interactively, else the file is located automatically
;		in the current Pilatus directory (manual location is still possible
;		using the keyword /PICK, in this case).  The rest is done in
;		SCAN_PD_PEAK, see there.  Calls SCAN_PD_SHOW.  Also calls ISNUM from 
;		MIDL.
; MODIFICATION HISTORY:
;		Created 10-JUNE-2008 by Mati Meron.
;		Modified 25-OCT-2008 by Mati Meron.  Added keyword SHOW.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	anam = 'alignment'
	ext = '.tif'
	if Isnum(exs) then begin
		if exs and not keyword_set(pic) and fildat.pdpath ne '' $
		then file = fildat.pdpath + anam + ext else file = ''
	endif else file = ''
	loc = Scan_PD_peak(file=file,ori=0,/bad,title=tit,_extra=_e)
	rfile = tit + ext
	dum = file_info(rfile)
	if keyword_set(sho) then Scan_PD_show, file= rfile, ori= 0, /bad, _extra= _e
	print
	print, '	Read file ' + rfile
	print, '	Created on ' + systime(0,dum.ctime,/utc)
	print
	print, '	In SPEC enter:'
	print, form = '("			SURF> pdx = ",i3)', loc[0]
	print, form = '("			SURF> pdy = ",i3)', loc[1]
	print

	return
end