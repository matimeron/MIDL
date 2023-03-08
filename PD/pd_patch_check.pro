Pro PD_patch_check, fir, sec, orientation=ori, raw= raw, angles=ang, tilt=til, $
	voffset= vof, lo_cut=lct, hi_cut= hct, force= frc, factors= mfac, _extra= _e

;+
; NAME:
;		PD_PATCH_CHECK
; VERSION:
;		8.0
; PURPOSE:
;		Checks Pilatus file patching.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_PATCH_CHECK, FIR, SEC [, keywords])
; INPUTS:
;	FIR
;		Scan number (may be associated with arbitrary number of PD frames.
;	SEC
;		Ditto.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ORIENTATION
;		Character string input specifying PD orientation.  Two possible inputs,
;		"horizontal" and "vertical" (first letter suffices). Alternatively can
;		be provided as numerical input, with 0 signifying horizontal and 1,
;		vertical.
;	/RAW
;		Switch.  If set, the data is read "raw", meaning the X and Y
;		coordinates are expressed in pixels, not angles or Q-values.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;	TILT
;		Optional detector tilt angle (couterclockwise) in degrees.  Valid for
;		angle and Q-xy coordinates, has no effect if /RAW is set.
;	VOFFSET
;		Numeric scalar, a multiplicative vertical offset to use when displaying
;		the scans.  Default is 1, i.e. no offset (other than this provided by
;		patching).
;	LO_CUT
;		Specifies the low cut of the data i.e. the level below which the data
;		should be cut.  For horizontal direction the low cut applies in the
;		horizontal dimension, and for vertical, in the vertical.  Given as
;		numbers in the [0,1] range, so that low cut of, say, 0.1, means "cut
;		data from 10% of the range and down".  The cut applies to raw data, 
;		before processing.
;
;		Note:	LO_CUT is never applied to FIR.
;	HI_CUT
;		Same as LOW_CUT, for the high end of the data.  Thus high cut of, say, 
;		0.8, means "cut data from 80% of the range and up".  The cut applies to
;		raw data, before processing.
;
;		Note:	HI_CUT is never applied to SEC.
;	/FORCE
;		Switch.  If set, forces junction even when there is no overlap between
;		some scans.  This involves extrapolation which greatly increases
;		uncertainties, so it should be used with care.
;	FACTORS
;		Optional output, see below.
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
;		Note:	/NORM is set by default in PD_PATCH_CHECK, it can be disabled 
;				by using NORM = 0 in the function call.
; OUTPUTS:
;		Standard output is plot only.  Optional output through keywords.
; OPTIONAL OUTPUT PARAMETERS:
;	FACTOR
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scan #i is multiplied to match with scan #(i-1).  The 0-th value
;		is always 1.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Standard patching using PD_PATCH_GEN.  Calls IMG_INT, SCAN_PD_READ,
;		SCAN_SCALE and SCAN_SHOW.  Calls DEFAULT and LEGEND_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 20-NOV-2007 by Mati Meron.
;		Modified 10-APR-2008 by Mati Meron.  Internal changes.
;		Modified 25-APR-2008 by Mati Meron.  Internal changes.
;		Modified 20-JUN-2008 by Mati Meron.  Internal changes.
;		Modified 15-APR-2009 by Mati meron.  Added keyword LO_CUT.
;		Modified 5-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 25-AUG-2010 by Mati Meron.  Replaced LEGEND with LEGEND_MM.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	xytit = [['pix.','Dth','Q!dxy!n'],['pix.','Beta','Q!dz!n']]
	if keyword_set(raw) or n_params() eq 0 then tn = 0 $
	else tn = 2 - keyword_set(ang)

	woff = Default(vof,1.,/dtyp)
	nin = Default(ori,1,/dtyp)

	res = PD_patch_gen(fir,sec,ori=ori,raw=raw,angles=ang,tilt=til,noin=nin,$
		lo_cut=lct,hi_cut=hct,force=frc,slist=slis,factors=mfac,_extra=_e)
	sfir = Scan_PD_read(slis[0],ori=ori,/norm,raw=raw,angles=ang,tilt=til, $
		noint=nin,_extra=_e)
	sfir = Img_int(sfir,xy_int=ori,z_int=1-ori,_extra=_e)
	ssec = Scan_PD_read(slis[1],ori=ori,/norm,raw=raw,angles=ang,tilt=til, $
		noint=nin,_extra=_e)
	ssec = Img_int(ssec,xy_int=ori,z_int=1-ori,_extra=_e)
	ssec = Scan_scale(ssec,mfac[1]*woff)

	lcol = [!pcol.green,!pcol.green,!pcol.blue,!pcol.blue]	
	temf = reform(sfir[0,*])
	minf = min(temf,max=maxf)
	tems = reform(ssec[0,*])
	mins = min(tems,max=maxs)
	ovl = [mins+ (maxs- mins)*Default(lct,0),minf+ (maxf- minf)*Default(hct,1)]
	
	dum = where(temf ge ovl[0] and temf le ovl[1],ndum)
	if ndum gt 0 then begin
		osfir = sfir[*,dum]
		lcol[1] = !pcol.purple
	endif else csfir = sfir
	dum = where(tems ge ovl[0] and tems le ovl[1],ndum)
	if ndum gt 0 then begin
		ossec = ssec[*,dum]
		lcol[3] = !pcol.purple
	endif else ossec = ssec

	tit = fildat.name + '  Scan #:' + strcompress(strjoin(string(slis),', '))
	stit = strcompress('Factor = ' + string(mfac[1],form='(f7.4)'))
	Scan_show, sfir, osfir, ssec, ossec, /ylog, lcol= lcol, $
	xtit= xytit[tn,ori], tit= tit, subtit= stit, ymar = [6,2], _extra= _e
	Legend_mm, text = ['Low Scan', 'Hi Scan', 'Patch Overlap'], line = [0,0,0], $
	color = [!pcol.green,!pcol.blue,!pcol.purple]

	return
end