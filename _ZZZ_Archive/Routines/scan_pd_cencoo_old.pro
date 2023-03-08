Function Scan_PD_cencoo_old, snum, fnum, verify = ver, raw = raw, angles = ang, $
	pinhole = pnh, offset = off 

;+
; NAME:
;		SCAN_PD_CENCOO
; VERSION:
;		8.32
; PURPOSE:
;		Returns center coordinates for area detector frames.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_CENCOO( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
;		A value of -1 is equivalent to "not given", useful for programming.
; KEYWORD PARAMETERS:
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	/RAW
;		Switch.  If set, the data is taken as "raw", meaning the X and Y
;		coordinates are expressed in pixels.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;	/PINHOLE
;		Switch.  Specifies pinhole angular transformation.  See SCAN_PD_READ for
;		details.
;	OFFSET
;		Scalar or 2 element vector, specifies center point offset, in pixels.
;		If given as scalar, it is taken as the X-offset.
; OUTPUTS:
;		Returns an array (type FLOAT) of dimension [2,NFRAMES] where NFRAMES is
;		the number of frames being processed.  Each row of the array represents
;		the center coordinates of one frame.  The result is in Q_xy, Q_z, 
;		coordinates unless /RAW or /ANGLES is set.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward, using the data using parameters stored in the FILDAT
;		structure in the common block.  Calls IMG_COO_CONV, SCAN_PD_CENTER, 
;		SCAN_PD_FRAMES, SCAN_Q_ANGS and SPEC_FILE_CHECK.  Also calls ISNUM and 
;		ONE_OF, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-NOV-2013 by Mati Meron.
;		Modified 15-DEC-2013 by Mati Meron.  Added keyword OFFSET.
;		Modified 10-SEP-2014 by Mati Meron.  Added keyword PINHOLE.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /sing, list = wsnum
	wfnum = Scan_PD_frames(wsnum,fnum,ver=ver,nfr=nfnum)
	cur = fildat.scan[wsnum]

	wha = One_of(raw,ang)
	rcen = (wcen = Scan_PD_center(wsnum))
	if Isnum(off) then wcen = wcen + ([off,0])[0:1]			
	res = fltarr(2,nfnum)
	res [0,*] = wcen[0]
	res[1,*] = wcen[1]
	if wha then begin
		angs = Scan_q_angs(wsnum,wfnum,/xrev)
		dist = cur.pdist + (cur.g_l)[3]*(1/cos(!dtor*angs[1,*]) - 1)
		if keyword_set(pnh) then pdst = cur.pdsdist else pdst = !null
		for i = 0, nfnum-1 do begin
			pdist = pdst
			res[*,i] = Img_coo_conv(res[0,i],res[1,i],cent=rcen,$
			alp=angs[0,i],bet=angs[1,i],dth=angs[2,i],ipix=cur.pdpix,$
			dist=dist[i],pdist=pdist,lam=cur.lambda,/xrev,qval=(1-wha)/2,/sign)
		endfor
	endif

	return, res
end