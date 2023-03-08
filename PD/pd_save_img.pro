Pro PD_save_img, snum, fnum, save_dir= sdr, png= png, verbose= vrb, _extra = _e

;+
; NAME:
;		PD_SAVE_IMG
; VERSION:
;		8.16
; PURPOSE:
;		Saves frame images.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		PD_SAVE_IMG, SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Scan number or a list of scan numbers, in any form recognizable by
;		RANGE_PROC.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Optional list of frame numbers, in any form acceptable by RANGE_PROC.
;		By default, all the frames within SNUM are used.
;
;		Note:	If some of the frame numbers are out of the range present,
;		they're ignored.  If there are no frame numbers within the range
;		present, an error message is issued.
;
;		Note:	If SNUM includes more than a single scan, FNUM is ignored.
; KEYWORD PARAMETERS:
; 	SAVE_DIR
; 		The path to the save directory.  If not given, will be querried for 
; 		interactively.
; 	/PNG
; 		Switch.  If set, the images are saved in PNG format.  Default is JPG.
; 	/VERBOSE
; 		Switch.  If set, a message containing the number of images saved and the
; 		save path is printed to the screen.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  This includes (among others) the following keywords,
;		affecting data readout:
;
;			HROI
;				Two element vector defining horizontal region of interest,
;				in *pixels*.
;			VROI
;				Two element vector defining vertical region of interest, in
;				*pixels*.
;			/BAD
;				Switch.  If set, faulty (very high count rate) pixels are
;				removed from the data.
;			/NORM
;				Switch.  If set, the data is normalized to monitor counts.
;
;		See SCAN_PD_READ for more details.
; OUTPUTS:
;		Images saved to the save directory. 
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data must exist.
; PROCEDURE:
;		Straightforward, Reads the data displayes it and saves the images.
;		Calls SCAN_PD_FRAMES, SCAN_PD_SHOW and SPEC_FILE_CHECK.  Calls FILE_GET,
;		FNAMPARSE and WIMG_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JUL-2012 by Mati Meron.
;-

	on_error, 1

	Spec_file_check, snum, /pildet, nscan = nscan, list = wsnum
	if nscan gt 1 then fnum = -1

	dum = Fnamparse(sdr,path=pat)
	wpat = File_get(pat,/dir,stat=sta)
	if not sta then message, 'No save path, cannot proceed!'

	jpfl = 1 - keyword_set(png)
	fcount = 0l
	for i = 0l, nscan-1 do begin
		wfnum = Scan_PD_frames(wsnum[i],fnum,/ver,nframes=nfnum)
		for j = 0l, nfnum-1 do begin
			Scan_pd_show, wsnum[i],wfnum[j], /indiv, /even, tit=tit, _extra=_e
			fname = Fnamparse(tit)
			sname = wpat + fname
			if jpfl then Wimg_mm, sname, /auto, /jpg, qual = 100 $
			else Wimg_mm, sname, /auto
			fcount = fcount + 1
			tit = !null
		endfor
	endfor

	if keyword_set(vrb) then print, 'Saved ' + string(fcount,form='(i0)') + $
	' images in ' + wpat

	return
end