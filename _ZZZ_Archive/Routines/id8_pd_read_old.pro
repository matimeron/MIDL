Function ID8_PD_read_old, filename = fnam, bad = bad, hroi = hroi, vroi = vroi, $
	det_start = dts, det_loc = dtl, pd_cent = cnt, alpha = alp, lambda = lam, $
	jimg = jmg, raw = raw, angles = ang, relaxed = rel, title= tit, show= sho,$
	_extra = _e

;+
; NAME:
;		ID8_PD_READ
; VERSION:
;		8.15
; PURPOSE:
;		Reads 8ID Pilatus Detector files.
; CATEGORY:
;		Data Input/Output.
; CALLING SEQUENCE:
;		Result = ID8_PD_READ( keywords )
; INPUTS:
; 		None.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	FILENAME
;		Name of a TIFF file to be read.  Optional, if not given, the name is
;		querried for interactively. 
;	/BAD
;		Switch.  If set, faulty (very high count rate) pixels are removed from
;		the data.
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;	VROI
;		Two element vector defining vertical region of interest, in *pixels*.
;	DET_START
;		Two element vector specifying the initial X and Z coordinates of the
;		detector, in mm.
;	DET_LOC
;		Two element vector, specifying the current X and Z coordinates of the
;		detector in mm.
;	PD_CENT
;		Two element vector, specifying the location, in pixels, of the direct 
;		beam intercept on the detector, measured when the detector is in its
;		DET_START position.
;	ALPHA
;		The Alpha angle of the incoming beam, in degrees.
;	LAMBDA
;		Photon wavelength, in Angstrem.
;	/JIMG
;		Switch.  Specifies "just image" data readout, meaning raw data with no
;		coordinates and no transformations.  Regions of interest may still be
;		applied.
;	/RAW
;		Switch.  If set, the data is read "raw", meaning the X and Y
;		coordinates are expressed in pixels, not angles or Q-values.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;
;		Note:	The default data coordinates are Q_xy and Q_z.
;	/RELAXED
;		Switch.  If set, the internal transformation from angles to Q-values is
;		unconstrained.  By default it is constrained to the internal data
;		region, to avoid edge inaccuracies.
;	TITLE
;		Optional output, see below.
;	/SHOW
;		Switch.  If set, the output data is also displayed to the screen.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.
; OUTPUTS:
;		Returns an array (type FLOAT) of dimension [4,PX,PY] where PX, PY are
;		the Pilatus horizontal (short) and vertical (long) pixel sizes.  The 
;		[0,*,*] and [1,*,*] pages of the array contain the X and Y coordinates 
;		(in pixel, angle or Q units).  The [2,*,*] page contains the data and 
;		the [3,*,*] page contains the statistical errors.
;
;		Note:	If /JIMG is set, the output is a 2D array of dimension [PX,PY].
;		Note2:	PX, PY may be smaller than the above if HROI and/or VROI is used
; OPTIONAL OUTPUT PARAMETERS:
;	TITLE
;		Returns the detector data filename to be used as a plot title, for the
;		use of calling routines.
; COMMON BLOCKS:
; 	None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data file must exist.
; PROCEDURE:
;		Straightforward, reads the data and processes it using the provided 
;		parameters.  Calls IMG_COO_CONV, IMG_EXORCISE, IMG_TRIG and 
;		SCAN_PD_SHOW from SPEC_LIB.  Calls ERREST from SURF_LIB.  Also calls 
;		FILE_GET and FNAMPARSE, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-MAR-2012 by Mati Meron, through a surgery on SCAN_PD_READ.
;-

	on_error, 1

	ms_dist = 486.	;	Mirror - Sample distance.
	sd_dist = 3922.	;	Sample - Detector distance.
	pix = 0.172
	rot = 6

	profl = 1 - keyword_set(jmg)
	rnam = File_get(fnam,_extra=_e)
	tit = Fnamparse(rnam)
	rdat = rotate(read_tiff(rnam),rot)
	if profl then edat = Errest(rdat)
	dim = (size(rdat))[1:2]

	if keyword_set(bad) then begin
		rdat = Img_exorcise(rdat,(2+2*bad)/bad,iter=bad,_extra=_e)
		if profl then edat = $
		Img_exorcise(edat,(1+3*bad)/(2*bad),iter=bad,_extra=_e)
	endif

	if n_elements(hroi) eq 2 then begin
		whroi = 0 > hroi < (dim[0]-1)
		whroi = [min(whroi,max=max),max]
	endif else whroi = [0,dim[0]-1]
	if n_elements(vroi) eq 2 then begin
		wvroi = 0 > vroi < (dim[1]-1)
		wvroi = [min(wvroi,max=max),max]
	endif else wvroi = [0,dim[1]-1]
	rdat = rdat[whroi[0]:whroi[1],wvroi[0]:wvroi[1]]

	if profl then begin
		edat = edat[whroi[0]:whroi[1],wvroi[0]:wvroi[1]]
		dim = (size(rdat))[1:2]
		res = fltarr([4,dim])
		res[0,*,*] = (whroi[0] + findgen(dim[0]))#replicate(1.,dim[1])
		res[1,*,*] = replicate(1.,dim[0])#(wvroi[0] + findgen(dim[1]))
		res[2,*,*] = rdat
		res[3,*,*] = edat
	endif else res = rdat

	if (profl - keyword_set(raw)) then begin
		cnt = cnt - ((dtl - dts)*[-1,1] + [0,ms_dist*tan(!dtor*alp)])/pix
		qvl = 1 - keyword_set(ang)
		min = 1 - keyword_set(rel)
		coord = Img_coo_conv(res[0:1,*,*], dist= sd_dist, ipix=pix, cent=cnt, $
		alp=alp,bet=0.,dth=0.,lam= lam, /xrev, qvals= qvl, /sign,  _extra= _e)
		if min(dim) gt 1 then begin
			rdat = Img_trig(reform(res[2,*,*]),coord,xtr=tqxy,ytr=tqz,min=min)
			edat = Img_trig(reform(res[3,*,*]),/last)
			res[0,*,*] = tqxy#replicate(1,dim[1])
			res[1,*,*] = replicate(1,dim[0])#tqz
			res[2,*,*] = rdat
			res[3,*,*] = edat
		endif else res[0:1,*,*] = coord
	endif

	if keyword_set(sho) then $
	Scan_pd_show, res, raw = raw, ang = ang, title = tit, _extra = _e

	return, res
end