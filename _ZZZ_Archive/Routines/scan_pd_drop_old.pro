Pro Scan_PD_drop_old, snum, fnum, verbose = vrb

;+
; NAME:
;		SCAN_PD_DROP
; VERSION:
;		8.21
; PURPOSE:
;		Removes frames from the data folder.
; CATEGORY:
;		SPEC data management.
; CALLING SEQUENCE:
;		SCAN_PD_DROP, SNUM, FNUM [/VERBOSE]
; INPUTS:
;	SNUM
;		Single scan number.
;	FNUM
;		List of frame numbers, in any form acceptable by RANGE_PROC.  Value of
;		-1 stands for all the frames of SNUM.
;		Note:	Unlike in most SCAN routines, FNUM is mandatory.	
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	/VERBOSE
;		Switch.  If set, list of the dropped frames is displayed to the screen.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None, other that the scan SNUM must exist and be a valid area detector
;		scan.
; PROCEDURE:
; 		Straightforward.  Moves the selected frames to the BAD_FRAMES subfolder
; 		of the area detector data folder.  If said subfolder doesn't exist, it
; 		is created in the process.
;		Calls SCAN_PD_FRAMES and SPEC_FILE_CHECK.  Calls STRPARSE_MM, from MIDL
;		and SDEP from imports.
; MODIFICATION HISTORY:
;		Created 15-SEP-2013 by Mati Meron.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	if n_elements(fnum) ne 0 then begin
		cur = fildat.scan[wsnum]
		wfnum = Scan_PD_frames(wsnum,fnum,/verify,nframes=nfnum)
		if nfnum gt 0 then begin
			dum = Strparse_mm(cur.pdfnam,'_',lis)
			zlen = strlen(lis[dum])
			flist = fildat.pdpath + strjoin(lis[0:dum-1],'_') + '_'
			suf = string(wfnum,strcompress('(I0'+string(zlen)+')',/rem))
			flist = flist + suf + cur.pdfext
			wdir = fildat.pdpath + 'bad_frames' + sdep(/ds)
			check = file_search(wdir,/test_dir,/mark_dir) eq wdir
			if not check then file_mkdir, wdir
			file_move, flist, wdir, /over, verbose = vrb
		endif else message, 'No such frames!'
	endif else message, 'Need to provide frame numbers, or -1 for "all frames"!'

	return
end