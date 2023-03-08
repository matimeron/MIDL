Pro PD_movie_old, snum, fnum, verify = ver, amax = amx, cmark= cmr, relative= rel,$
	small= sml, wait= wai, wnew= new, save= sav, file=fnam, rate=rat, _extra= _e

;+
; NAME:
;		PD_MOVIE
; VERSION:
;		8.334
; PURPOSE:
;		Displays frames from a single Pilatus detector scan as a "movie".
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		PD_MOVIE, SNUM [, FNUM] [, keywords])
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
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	AMAX
;		Scalar value, absolute reference to be used as maximum for all the
;		images displayed.  If not provided, the maximal value of all the frames
;		read is used.
;	/CMARK
;		Switch.  Set by default, causes the center of the frame to be marked
;		with a cross and a vertical line passing through this cross.  Setting
;		CMARK explicitly to zero disables it.  Not active when 1D data is
;		displayed.
;
;		Note:	If CMARK=2 is used, the vertical line is omitted.
;	/RELATIVE
;		Switch.  If set, the XY_REG and/or Z_REG limits (when used) are taken 
;		relative to the frame center.
;	/SMALL
;		Switch.  If set, the display is shrunk by roughly a factor of 2.
;	/WAIT
;		Switch.  If set, the routine waits for user input ("any key") to advance
;		to the next frame.  Alternatively, if WAIT is given as negative value, 
;		the routine waits for the time specified by the absolute value of WAIT
;		before displaying the next frame.
;
;		Note:	In the positive WAIT mode, the following keys have special role:
;				"P"	-	Go back one frame.
;				"Q"	-	Quit.
;	/WNEW
;		Switch.  If set, a new graphics window is created instead of the old
;		one being overwritten.
;	/SAVE
;		Switch.  If set, the sequence of frames displayed is saved as an MPEG
;		file (i.e. a movie).  The save file is selected interactively (unless 
;		provided through the keyword FILE).
;	FILE
;		Accepts a full or partial name for the save file.  In case of partial 
;		name the missing components will be selected interactively.
;	RATE
;		Sets the display rate of the movie, in frames/sec.  Default is 1.
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
;
;		Note:	Unlike the other PD routines, PD_MOVIE doesn't normalize data by
;				default.
; OUTPUTS:
;		Normally graphics output only, either as 2D images or as plots of 1D 
;		integrated data.  Optionally the frames displayed can be saved as a  
;		movie (see /SAVE).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data must exist.
; PROCEDURE:
;		Straightforward, Reads the data and displayes it as a set of images or
;		1D curves.  Calls SCAN_PD_FPREAD, SCAN_PD_FRAMES, SCAN_PD_SHOW and 
;		SPEC_FILE_CHECK.  Calls DEFAULT, FILE_GET and STREQ, from MIDL.
; MODIFICATION HISTORY:
;		Created 30-MAR-2009 by Mati Meron.
;		Modified 15-AUG-2009 by Mati Meron.  Added keyword SAVE.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 10-MAR-2010 by Mati Meron.  Internal changes using elements 
;		from Tim Graber's BEAM_JITTER.  Added keywords RATE and FILE.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 15-NOV-2013 by Mati Meron.  Added keywords CMARK and RELATIVE.
;		Modified 5-FEB-2014 by Mati Meron.  Made CMARK on the default option.
;		Modified 20-NOV-2014 by Mati Meron.  Added keyword SMALL.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	brep = 30

	Spec_file_check, snum, /pildet, nscan = nscan, list = wsnum
	if nscan gt 1 then fnum = -1

	amx = Default(amx,0)
	if amx eq 0 then for i = 0, nscan-1 do $
		amx = amx > max(Scan_PD_fpread(wsnum[i],fnum,ver=ver,_extra=_e))
	wai = Default(wai,-1e-6,/dtyp)
	if wai gt 0 then print, string([13b,9b,9b]) + $
	'Hit "Q" to exit, "P" for previous, any other key to continue'
	sfl = keyword_set(sav) and wai le 0

	win = keyword_set(new) or (sfl and keyword_set(sml))
	wcmr = Default(cmr,1,/dtyp)
	rpd = keyword_set(rel)
	fcount = 0l
	for i = 0l, nscan-1 do begin
		wfnum = Scan_PD_frames(wsnum[i],fnum,verify=ver,nframes=nfnum)
		for j = 0l, nfnum-1 do begin
			Scan_pd_show, wsnum[i],wfnum[j],amax= amx,wnew= win, /indiv, /even,$
			cmark= wcmr, rel= rel, small= sml, expad= rpd*[8,4], shave= sml,$
			_extra= _e
			rpd = 0
			if sfl then begin
				if fcount eq 0 then begin
					frep = round(brep/Default(rat,1)) > 1
					handle = mpeg_open([!d.x_size,!d.y_size],_extra=_e)
				endif
				mpeg_put, handle, window= !d.window, frame= fcount*frep+1, /ord
			endif
			if wai gt 0 then begin
				dum = (dum = get_kbrd())
				if Streq(dum,'p',1) then j = (j-2) > (-1) $
				else if Streq(dum,'q',1) then break
			endif else if wai lt 0 then wait, abs(wai)
			fcount = fcount + 1
			win = 0
		endfor
	endfor
	if sfl then begin
		mpeg_put, handle, window= !d.window, frame= 0, /ord
		mpfile = File_get(fnam,stat=stat,/write,/over,def='mpg',_extra=_e)
		if stat then mpeg_save, handle, file = mpfile
		mpeg_close, handle
	endif

	if wai gt 0 then print, string([13b,9b,9b,9b]) + 'The End'
	return
end