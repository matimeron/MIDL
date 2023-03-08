Pro Scan_PD_restore, snum, fnum, verbose = vrb

;+
; NAME:
;		SCAN_PD_RESTORE
; VERSION:
;		8.21
; PURPOSE:
;		Restores frames which were removed using SCAN_PD_DROP.
; CATEGORY:
;		SPEC data management.
; CALLING SEQUENCE:
;		SCAN_PD_RESTORE, SNUM, FNUM [/VERBOSE]
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		List of frame numbers, in any form acceptable by RANGE_PROC.  If not
;		given, or given as -1 , all the dropped frames of SNUM are restored.  
; KEYWORD PARAMETERS:
;	/VERBOSE
;		Switch.  If set, list of the restored frames is displayed to the screen.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
;		PREV_READ.  See the routine SCAN_PD_FPREAD for description.
;		SCAN_PD_RESTORE sets the first entry of PREV_READ to -1, assuring that
;		the data will be reread, following the frame restoration.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The scan SNUM must exist and be a valid area detector scan.
;		The BAD_FRAMES subfolder of the area detector data folder must exist.
; PROCEDURE:
; 		Straightforward.  Moves the selected frames from the BAD_FRAMES 
; 		subfolder to the area detector data folder.
;		Calls SPEC_FILE_CHECK.  Calls ARREQ, RANGE_PROC and STRPARSE_MM, from 
;		MIDL and SDEP from IMPORTS.
; MODIFICATION HISTORY:
;		Created 15-SEP-2013 by Mati Meron.
;		Modified 30-MAR-2014 by Mati Meron.  Internal changes.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common prev_read, lsnum, ldim, lwfnum, lres
	on_error, 1

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	cur = fildat.scan[wsnum]
	wdir = fildat.pdpath + 'bad_frames' + sdep(/ds)
	check = file_search(wdir,/test_dir,/mark_dir) eq wdir
	if check then begin
		dum = Strparse_mm(cur.pdfnam,'_',lis)
		zlen = strlen(lis[dum])
		flist = wdir + strjoin(lis[0:dum-1],'_') + '_'
		if n_elements(fnum) gt 0 then begin
			wfnum = Range_proc(fnum,/unique)
			if Arreq(wfnum,[-1]) then suf = '*' $
			else suf = string(wfnum,strcompress('(I0'+string(zlen)+')',/rem))
		endif else suf = '*'
		flist = flist + suf + cur.pdfext
		flist = file_search(flist,count=con)
		if con gt 0 then begin
			file_move, flist, fildat.pdpath, /over, verbose= vrb
			if Isnum(lsnum) then lsnum = -1l
		endif else message, 'Nothing to restore', /con
	endif else message, 'Bad_frames folder not found!'

	return
end