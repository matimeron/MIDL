Function Scan_PD_read, snum, fnum, filename = fnam, verify = ver, $
	orientation = ori, outmode = otm, rotate = rot, fcoeffs = fco, $
	norm = nrm, globnorm = glb, sin_norm = snr, per_frame = prf, $
	bad = bad, grid_corr = grc, hroi = hroi, vroi = vroi, pol_corr = plc, $
	jimg = jmg, raw = raw, angles = ang, tilt= til, relaxed= rel, nointer= nin,$
	cxy_corr= cxy, indiv_frame= inf, pinhole= pnh, title=tit, ext=ext, _extra=_e

;+
; NAME:
;		SCAN_PD_READ
; VERSION:
;		8.476
; PURPOSE:
;		Reads Area Detector files.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_PD_READ( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.  Mandatory when reading from a SPEC file, but not to
;		be used when reading a TIFF or SFRM file directly.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number within the scan.  If not given, defaults to all frames.
;		A value of -1 is equivalent to "not given", useful for programming.
; KEYWORD PARAMETERS:
;	FILENAME
;		Name of a TIFF or SFRM file to be read directly (not using SPEC data).
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	ORIENTATION
;		An optional input specifying the detector orientation.  Can be provided
;		as integer input, 0 for horizontal, 1 for vertical.  Alternatively, can
;		also be provided as character input with two possible values, "HOR"
;		(for horizontal) and "VER" (for vertical).  First letter is sufficient.
;		Default is vertical.
;	OUTMODE
;		An optional input specifying the detector column arm used.  Can be
;		provided as integer input, 0 for the "XR" arm, 1 for "GID".
;		Alternatively can be provided as character input with two possible
;		values, "XR" and "GID".  Default is "GID".
;
;		Note:	The default values of these two values are taken from the
;		assigned SPEC file, if one exists.
;
;		Note:	Both keywords above also serve as optional outputs, returning
;		the orientation and detector column arm values actually used.  This is
;		true even if no assigned SPEC file exists, though in such case the
;		values have no effect.
;	ROTATE
;		Integer scalar, specifies rotation as in the IDL ROTATE function.
;		Optional, default is established internally.
;	FCOEFFS
;		List of optional coefficients by which the individual frames are to be
;		multiplied while being read.  By default all coefficients are 1.
;	/NORM
;		Switch.  If set, the data is normalized to monitor counts, scaled by
;		the average of monitor counts for the scan.  The scaling can be changed,
;		see keyword /GLOBNORM, below.
;	/GLOBNORM
;		Switch.  If set, the normalization is "global", i.e. to the value of
;		MONC alone, with no scaling.  This yields faithful "global
;		normalization" across multiple scans, at the price of the data not being
;		approximately equivalent to numbers of counts.
;
;		Note:	If NORM is not set, /GLOBNORM has no effect.
;	/SIN_NORM
;		Disabled on 5/11/09, as not needed.
;	/PER_FRAME
;		Switch.  If set, the data is normalized to number of frames evaluated.
;		This is done independent of any other normalization.
;	/BAD
;		Switch.  If set, faulty (very high count rate) pixels are removed from
;		the data.
;	GRID_CORR
;		Specifies correction for the grid present in Pilatus detector data.  If
;		given as a value such that |GRID_CORR| < 1, the data within the grid
;		pixels is divided by (1 + GRID_CORR).  If given as 1 (or set as 
;		/GRID_CORR), the last used value (0 if non was used) is reused.  If not
;		given or set, no correction is performed.
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;	VROI
;		Two element vector defining vertical region of interest, in *pixels*.
;	/POL_CORR
;		Switch.  Set by default, causes the data to be corrected for 
;		polarization.  Assumes initial horizontal polarization.  Can be disabled
;		by setting POL_CORR explicitly to zero.
;	/JIMG
;		Switch.  Specifies "just image" data readout, meaning raw data with no
;		coordinates and no transformations.  Regions of interest may still be
;		applied.
;	/RAW
;		Switch.  If set, the data is read "raw", meaning the X and Y
;		coordinates are expressed in pixels, not angles or Q-values.
;
;		Note:	When reading directly from a TIFF or SFRM file, the data is 
;				always raw.  When reading through a SPEC file, the default 
;				coordinates are Q_xy and Q_z.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;
;		Note:	In case of direct read from TIFF or SFRM file, /ANGLES has no 
;		effect.
;		
;		Note:	When reading a camera image, /JIMG is automatically set and all
;		other options for image processing are disabled.
;	TILT
;		Optional detector tilt angle (couterclockwise) in degrees.  Valid for
;		angle and Q-xy coordinates, has no effect if /JIMG or /RAW is set.
;	/RELAXED
;		Switch.  If set, the internal transformation from angles to Q-values is
;		unconstrained.  By default it is constrained to the internal data
;		region, to avoid edge inaccuracies.
;	/NOINTER
;		Switch.  If set, data is not reinterpolated to a rectangular grid,
;		following a transformation to angles or q-values.  Default is to
;		reinterpolate.
;	CXY_CORR
;		Switch or 2 element vector.  If set and if the "far detector" flag is 
;		set, causes the detector center location to be corrected for motion of 
;		the CCD_X and CCD_Y motors.  Two possible options:
;			1	-	If CXY_CORR is set as a switch, the current locations of
;					CCD_X and CCD_Y are used as is, i.e. the reference point
;					is assumed to be [0,0]
;			2	-	If CXY_CORR is given as 2-element vector, it is assumed to 
;					be the reference point to which CCD_X and CCD_Y are compared
;	/INDIV_FRAME
;		Switch.  If set, and if FNUM points to a single frame, the angles 
;		corresponding to this frame (which may differ from the global angles
;		for the scan) are used.  If /INDIV_FRAME is set but FNUM points to
;		multiple frames, no action is taken and a warning is issued.
;	PINHOLE
;		Specifies that horizontal pinhole angle transformations are to be used.
;		If set or provided with any positive value, the distance PDSDIST from
;		the structure FILDAT (see common block) is used in the transformation.
;		If provided with any negative value, PDSTAT - PDSDIST is used (legacy
;		setting).  If not set, or provided with 0 value, pinhole transformation
;		is not applied.  If /JIMG or /RAW is set, PINHOLE has no effect.
;	TITLE
;		Optional output, see below.
;	EXT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.
; OUTPUTS:
;		Returns an array (type FLOAT) of dimension [4,PX,PY] where PX, PY are
;		the Pilatus horizontal (short) and vertical (long) pixel sizes (if
;		"HOR" orientation is used, the dimensions are reversed).  The [0,*,*]
;		and [1,*,*] pages of the array contain the X and Y coordinates (in pixel
;		angle or Q units).  The [2,*,*] page contains the data and the [3,*,*]
;		page contains the statistical errors.
;
;		Note:	If /JIMG is set, the output is a 2D array of dimension [PX,PY].
;		Note2:	PX, PY may be smaller than the above if HROI and/or VROI is used
; OPTIONAL OUTPUT PARAMETERS:
;	TITLE
;		Returns the detector data filename to be used as a plot title, for the
;		use of calling routines.  If multiple frames are summed, the name has
;		SUM in place of frame number.
;	EXT
;		Returns the extension of the data file(s).
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
;		PGRID_CORR.  Holds the Pilatus grid correction value and the 
;		corresponding correction array.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward, reads the data using parameters stored in the FILDAT
;		structure in the common block.  Calls IMG_COO_CONV, IMG_EXORCISE,
;		IMG_TRIG, SCAN_COLUMN, SCAN_GET_ANGS, SCAN_PAR_READ, SCAN_PD_CENTER, 
;		SCAN_PD_FAROFF, SCAN_PD_FRAMES and SCAN_Q_ANGS.  Calls READ_BIS from 
;		APEX.  Calls ERREST from SURF_LIB.  Also calls ARREQ, DEFAULT, 
;		FILE_GET, FNAMPARSE, ISNUM, RANGE_COMP, SIGN, STREQ, STRMATCH_MM, 
;		STRPARSE_MM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-AUG-2007 by Mati Meron.
;		Modified and documented 30-OCT-2007 by Mati Meron.
;		Modified 10-Nov-2007 by Mati Meron.  Internal changes.
;		Modified 15-NOV-2007 by Mati Meron.  Added keyword BAD.
;		Modified 25-MAR-2008 by Mati Meron.  Added keyword PINHOLE.
;		Modified 10-APR-2008 by Mati Meron.  Added keyword NOINTER and made
;		some internal changes.
;		Modified 15-APR-2008 by Mati Meron.  Added keyword JIMG.
;		Rewritten 25-APR-2008 by Mati Meron.  Streamlined and functionally
;		combined with the previous SCAN_PD_SUM.  Added keyword FCOEFFS.
;		Modified 15-JUN-2008 by Mati Meron.  Added keyword GLOBNORM.
;		Modified 20-JUN-2008 by Mati Meron.  Internal changes.
;		Modified 20-AUG-2008 by Mati Meron.  Internal changes.  Added keyword
;		OUTMODE.
;		Modified 30-OCT-2008 by Mati Meron.  Internal changes.
;		Modified 25-FEB-2009 by Mati Meron.  Internal changes.
;		Modified 5-MAR-2009 by Mati Meron.  Added keyword SIN_NORM.
;		Modified 5-APR-2009 by Mati Meron.  Added keyword INDIV_FRAME.
;		Modified 25-JUN-2009 by Mati Meron.  Added keyword PER_FRAME and 
;		internal changes.
;		Modified 10-AUG-2009 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2009 by Mati Meron.  Internal changes.
;		Modified 5-NOV-2009 by Mati Meron.  Disabled keyword SIN_NORM.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Added APEX support.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 30-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 10-JUN-2010 by Mati Meron.  Added keyword GRID_CORR and its
;		corresponding common block, PGRID_CORR.
;		Modified 5-FEB-2011 by Mati Meron.  Internal changes.  Added far 
;		detector support.
;		Modified 15-FEB-2011 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2011 by Mati Meron.  Updated error calculations for APEX
;		Modified 10-JUN-2011 by Mati Meron.  Removed dependence on SCAN_GET_ANGS
;		Modified 15-JUN-2011 by Mati Meron.  Restored SCAN_GET_ANGS for 
;		patological cases.
;		Modified 20-JUL-2011 by Mati Meron.  Internal changes.
;		Modified 10-OCT-2011 by Mati Meron.  Internal changes, added camera
;		support.
;		Modified 25-OCT-2011 by Mati Meron.  Added keyword ROTATE.
;		Modified 20-MAR-2012 by Mati Meron.  Added keyword CXY_CORR.
;		Mofified 15-JUN-2012 by Mati Meron.  Internal changes.
;		Modified 15-FEB-2014 by Mati Meron.  Added keyword POL_CORR.
;		Modified 10-JUL-2015 by Mati Meron.  Internal changes.
;		Modified 25-JUL-2015 by Mati Meron.  Internal changes.
;		Modified 5-APR-2016 by Mati Meron.  Added keyword EXT.
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;		Modified 5-OCT-2016 by Mati Meron.  Internal changes.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common pgrid_corr, cval, carr
	on_error, 1

	cent=Scan_PD_center(snum,ori=ori,out=otm,rot=rot,dim=dim,stat=sta,_extra=_e)
	profl = 1 - keyword_set(jmg)

	if sta then begin
		cur = fildat.scan[snum]
		apfl = (cur.pdstat - 3)/2
		if apfl eq 2 then profl = 0
		wfnum = Scan_PD_frames(snum,fnum,verify=ver,nframes=nfnum,_extra=_e)
		wco = Default(fco,replicate(1.,nfnum),/dtyp)
		if n_elements(wco) ge nfnum then wco = wco[0:nfnum-1] $
		else message, 'Insufficient number of coefficients!'
		if keyword_set(nrm) then begin
			ncol = ''
			if Type(nrm) eq 7 then ncol= nrm else if nrm eq 1 then ncol= 'monc'
			if ncol ne '' then begin
				col = Scan_column(snum,ncol)
				if Streq(ncol,'monc') and min(col) eq 0 then begin
					acol = Scan_column(snum,'amonc',sta=csta)
					if csta then col = acol
				endif
				if keyword_set(glb) then sca= 1 else sca= Total(col)/cur.ncr[1]
				wco = sca*wco/col[wfnum]
			endif
		endif
		dum = Strparse_mm(cur.pdfnam,'_',lis)
		zlen = strlen(lis[dum])
		fcod = strcompress('(I0'+string(zlen)+')',/rem)
		gnam = fildat.pdpath + strjoin(lis[0:dum-1],'_') + '_'
		ext = cur.pdfext
		rnam = gnam + string(wfnum,fcod) + ext
		if nfnum eq 1 then tit = gnam + string(wfnum,fcod) $
		else tit = gnam + 'SUM' + '(' + Range_comp(wfnum) + ')'
		rdat = (edat = 0)
		for i = 0l, nfnum-1 do begin
			case apfl of
				0	:	dat = rotate(read_tiff(rnam[i]),rot)
				1	:	dat = Read_BIS(rnam[i],rot)
				2	:	dat = Rimg_mm(rnam[i],rot=rot)
				3	:	message, 'Unknown!'
				4	:	dat = rotate(read_tiff(rnam[i]),rot)
			endcase
			dat = dat > 0
			rdat = rdat + wco[i]*dat
			if profl then edat = edat + wco[i]^2*Errest(dat,emod=apfl,/squ)
		endfor
		if profl then edat = sqrt(edat)
		if keyword_set(prf) then begin
			rdat = rdat/nfnum
			if profl then edat = edat/nfnum
		endif
	endif else begin
		pext = ['.tif','.sfrm','.tiff','.jpeg','.jpg']
		raw = 1
		rnam = File_get(fnam,_extra=_e)
		tit = strmid(rnam,0,strpos(rnam,'.'))
		dum = Fnamparse(rnam,ext=ext)
		apfl = Strmatch_mm(ext,pext) < 2
		if apfl eq 2 then profl = 0
		case apfl of
			0	:	begin
						rdat = rotate(read_tiff(rnam),rot)
						if (file_info(rnam)).size eq (4l*981*1043 + 4096l) $
						then apfl = 4
					end
			1	:	rdat = Read_BIS(rnam,rot)
			2	:	rdat = Rimg_mm(rnam,rot=rot)
		endcase
		rdat = rdat > 0
		if profl then edat = Errest(rdat,emod=apfl)
		dim = (size(rdat))[1:2]
	endelse

	if keyword_set(bad) then begin
		rdat = Img_exorcise(rdat,(2.+2*bad)/bad,iter=bad,_extra=_e)
		if profl then edat = $
		Img_exorcise(edat,(1.+3*bad)/(2*bad),iter=bad,_extra=_e)
	endif

	if keyword_set(grc) and apfl eq 0 then begin
		adim = [195,487]
		scheck = Arreq(dim,adim) + 2*Arreq(dim,reverse(adim))
		if scheck gt 0 then begin
			if not Isnum(cval) then begin
				cval = 0.
				carr = make_array(dim,val=1.)
			endif
			grow = [ 59, 60, 61,  120,121,122,  181,182,183,  242,243,244, $
					303,304,305,  364,365,366,  425,426,427]
			gcol = [ 96, 97, 98]
			if scheck eq 2 then begin
				tem = grow
				grow = gcol
				gcol = tem
			endif
			grfl = 1
			if grc ne 1 then begin
				if grc ne cval then begin
					if abs(grc) lt 1 then begin 
						cval = grc
						carr[gcol,*] = 1. + cval
						carr[*,grow] = 1. + cval
					endif else begin
						message, 'Grid correction value too large', /con
						grfl = 0
					endelse
				endif
			endif
			if grfl and cval ne 0 then begin
				rdat = rdat/carr
				if profl then edat = edat/carr
			endif
		endif else message, 'Unrecognized size, aborting grid correction', /con
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
		profl = profl - keyword_set(raw)
	endif else res = rdat

	if profl then begin
		if keyword_set(inf) and nfnum gt 1 then message, $
		'Individual option is only for single frames', /con
		angs = Scan_q_angs(snum,wfnum,/constant,/warn,/xrev,stat=sta,_extra=_e)
		if not sta then begin
			angs = Scan_get_angs(snum,wfnum,/warn)
			message, 'Warning: reading angles from header', /con
		endif
		dist = cur.pdist + (cur.g_l)[3]*(1/cos(!dtor*angs[1]) - 1)
		case Sign(Default(pnh,angs[2] gt 1,/dtyp)) of
			-1	:	pdist = cur.pdist - cur.pdsdist
			0	:	pdist = 0.
			1	:	pdist = cur.pdsdist
		endcase
		if cur.pdfar then begin
			cent = cent - Scan_PD_faroff(snum,alp=angs[0])
			if keyword_set(cxy) then begin
				if n_elements(cxy) eq 2 then ref = cxy else ref = [0.,0.]
				cx = Scan_par_read(snum,'CCD_X') - ref[0]
				cy = Scan_par_read(snum,'CCD_Y') - ref[1]
				cent = cent - [-cx,cy]/cur.pdpix
			endif
		endif
		qvl = 1 - keyword_set(ang)
		min = 1 - keyword_set(rel)
		coord = Img_coo_conv(res[0:1,*,*], dist= dist, pdist= pdist, $
			ipix=cur.pdpix, cent=cent, alp=angs[0],bet=angs[1],dth=angs[2],$
			tau= til, lam= cur.lambda, /xrev, qvals= qvl, /sign, $
			ret_dth = rdth, ret_bet = rbet, _extra= _e)
		if Default(plc,1,/dtyp) then begin
			pfac = 1 - (sin(!dtor*rdth)#cos(!dtor*rbet))^2
			res[2,*,*] = res[2,*,*]/pfac
			res[3,*,*] = res[3,*,*]/pfac
		endif
		if not keyword_set(nin) and min(dim) gt 1 then begin
			rdat = Img_trig(reform(res[2,*,*]),coord,xtr=tqxy,ytr=tqz,min=min)
			edat = Img_trig(reform(res[3,*,*]),/last)
			res[0,*,*] = tqxy#replicate(1,dim[1])
			res[1,*,*] = replicate(1,dim[0])#tqz
			res[2,*,*] = rdat
			res[3,*,*] = edat
		endif else res[0:1,*,*] = coord
	endif

	return, res
end