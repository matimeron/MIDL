Function Scan_PD_ver_old, snum, fnum, warn = wrn, stat = sta

;+
; NAME:
;		SCAN_PD_VER
; VERSION:
;		8.31
; PURPOSE:
;		Verifies the existance of Pilatus detector file(s).
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_PD_VER( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Optional list of frame numbers, in any form acceptable by RANGE_PROC.
;		By default, all the frames within SNUM are used.
; KEYWORD PARAMETERS:
;	/WARN
;		Switch.  If set, warnings about missing frame files are printed to the
;		screen.
;	STATUS
;		Optional output, see below.
; OUTPUTS:
;		Returns a sublist of FNUM, with the numbers of *valid* frames.  If none
;		are found, returns -1.
; OPTIONAL OUTPUT PARAMETERS:
;	STATUS
;		Returns a status code for the verification,  The following codes are
;		used:
;			0	-	Some or all data missing.
;			1	-	Data present and complete.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, Generates the frame filenames using data stored in the
;		SPEC file and checks whether said frames exist and are not corrupted.  
;		Calls SCAN_PD_FRAMES and SPEC_FILE_CHECK.  Calls STRPARSE_MM, from MIDL.
;
;		Note:	In the case of APEX frames, the check is only for existance.
; MODIFICATION HISTORY:
;		Created 15-NOV-2007 by Mati Meron.
;		Modified 25-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 5-FEB-2010 by Mati Meron, to check not only for existance but
;		also for instances of corrupted files.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 5-APR-2010 by Mati Meron.  Internal changes, APEX related.
;		Modified 10-OCT-2011 by Mati Meron.  Internal changes, camera related.
;		Modified 30-MAR-2014 by Mati Meron.  Internal changes.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	tsiz = 4l*195*487 + 4096l
	wfl = keyword_set(wrn)
	sta = 0
	res = -1l

	Spec_file_check, snum, /sing, /pil
	cur = fildat.scan[snum]
	apfl = (cur.pdstat - 3)/2
	wfnum = Scan_PD_frames(snum,fnum,ver=0,nframes=nfr)
	starr = bytarr(nfr)
	dum = Strparse_mm(cur.pdfnam,'_',lis)
	zlen = strlen(lis[dum])
	fcod = strcompress('(I0'+string(zlen)+')',/rem)
	for i = 0, nfr-1 do begin
		lis[dum] = string(wfnum[i],fcod)
		rnam = fildat.pdpath + strjoin(lis,'_') + cur.pdfext
		sver = file_info(rnam)
		starr[i] = sver.exists and ((sver.size eq tsiz) or (apfl gt 0))
		if wfl and starr[i] eq 0 then message, $
		'Frame ' + rnam + ' is corrupted or missing', /cont
	endfor
	sta = min(starr)
	dum = where(starr eq 1, ndum)
	if ndum gt 0 then res = wfnum[dum]

	return, res
end