Function Scan_PD_ftod, snum, fnum, verify = ver, xdet = xdt, yfot = yft, $
	round = rnd, radians = rad, dang = dan 

;+
; NAME:
;		SCAN_PD_FTOD
; VERSION:
;		8.215
; PURPOSE:
;		Converts from locations on beam footprint to locations on detector (for
;		pinhole geometry).
; CATEGORY:
;		Liquid Surface data analysis.
; CALLING SEQUENCE:
;		Result = SCAN_PD_FTOD( SNUM [FNUM] [,keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Optional list of frame numbers, in any form acceptable by RANGE_PROC.
;		By default, all the frames within SNUM are used.
; KEYWORD PARAMETERS:
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	XDET														|
;		Scalar, the x-value for a point at the detector, 		| One and only
;		*in pixels*.  The value is measured from the detector	| one of these
;		center (see SCAN_PD_CENTER) along the mathematically	| two must be
;		positive direction.										|
;	YFOT														|
;		Scalar, the location of a point along the beam	 		|
;		footprint, *in mm*.  The value is measured from the		|
;		center of the footprint, with positive direction		|
;		being downstream.										|
;	/ROUND
;		Switch.  If set, the XDET values (whether on input or output) are 
;		rounded to the nearest pixel.
;	/RADIANS
;		Switch.  If set, the DANG (see below) values are returned in radians.
;		Default is degrees.
;	DANG
;		Optional output, see below.
; OUTPUTS:
; 		If XDET is provided, returns the corresponding values of YFOT, and vice
; 		versa.  The output is a vector with one entry for each frame processed.
; 		If only a single frame is processed, the output is scalar.
; 		
; 		Note:	XDET values are in pixels, YFOT values are in mm.
; OPTIONAL OUTPUT PARAMETERS:
;	DANG
;		Returns the angle offset(s) (from the nominal DTH angle of scan SNUM) 
;		corresponding to the XDET or YFOT values provided.  The result is in 
;		degrees, unless RADIANS is set.
;		
;		Note:	If /ROUND (see above) is set, the XDET values are rounded 
;		*prior* to the calculation of DANG.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The point YDET*cos(dth) = L1, where dth is the horizontal angle for
;		a frame (as recorded in the SPEC file) and L1 is the sample-slit 
;		distance (also from the SPEC file) is a singularity for the 
;		transformation.  Therefore YDET*cos(dth) < L1 is required for all frames
; PROCEDURE:
;		Straightforward, reads the data using parameters stored in the FILDAT
;		structure in the common block, if the structure is defined.  Calls
;		SCAN_PD_FRAMES, SCAN_Q_ANGS and SPEC_FILE_CHECK.  Also calls FPU_FIX and
;		ONE_OF, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-NOV-2013 by Mati Meron.
;		Modified 20-NOV-2013 by Mati Meron.  Added keyword DANG.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	rnfl = keyword_set(rnd)
	if keyword_set(rad) then amult = 1. else amult = !radeg

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	wfnum = Scan_PD_frames(wsnum,fnum,verify=ver,nframes=nfnum)
	cur = fildat.scan[wsnum]

	phi = reform((Scan_q_angs(wsnum,wfnum,/xrev,/rad))[2,*])
	l1 = cur.pdist - cur.pdsdist
	l2 = cur.pdsdist

	wha = One_of(xdt,yft,val=xory)
	if n_elements(xory) eq 1 then xory = xory[0] $
	else message, 'Input (XDET or YFOT) must be a scalar!'

	case wha of
		-1	:	message, 'Missing input!
		0	:	begin
					if rnfl then xory = round(xory)
					xd = fildat.pdpix*xory
					res = l1*xd/(l2*sin(phi) + xd*cos(phi))
				end
		1	:	begin
					if max(xory*cos(phi)) lt l1 then yf = xory $
					else message, 'Impossible input values, exiting!'
					xd = l2*yf*sin(phi)/(l1 - yf*cos(phi))
					res = xd/fildat.pdpix
					if rnfl then begin
						res = round(res)
						xd = res*fildat.pdpix
					endif
				end
	endcase

	dan = amult*atan(xd/l2)
	if nfnum eq 1 then begin
		res = res[0]
		dan = dan[0]
	endif else if n_elements(dan) eq 1 then dan = replicate(dan[0],nfnum)

	return, FPU_fix(res)
end