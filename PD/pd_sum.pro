Pro PD_sum, snum, fnum, warn_only = won, orientation= ori, $
	raw= raw, angles = ang, tilt = til, title = tit, display = dsp, cmark= cmr,$
	xy_reg= xyr, z_reg= zr, xy_int= xyi, z_int= zi, result = res, _extra = _e

;+
; NAME:
;		PD_SUM
; VERSION:
;		8.32
; PURPOSE:
;		Displays a sum of Pilatus detector scans and frames.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		PD_SUM, SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Scan number or a list of scan numbers, in any form recognizable by
;		RANGE_PROC.
;
;		Note:	Alternatively, SNUM may be data from a preprocessed scan or
;		scans, in which case it is displayed as is.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Optional list of frame numbers, in any form acceptable by RANGE_PROC.
;		By default, all the frames within SNUM are used.
;
;		Note:	If some of the frame numbers are out of the range present,
;		they're ignored.  If there are no frame numbers within the range
;		present, an error message is issued.
; KEYWORD PARAMETERS:
;	/WARN_ONLY
;		Switch.  In routine operation, if any of the parameters In_rot,
;		Out_rot, Det_th (aka Alpha, Beta, Dth) is not constant for all the scans
;		in SNUM, PD_SUM exists with an error message.  When /WARN_ONLY is set,
;		only a warning message is issued and the evaluation continues.
;
;		Important:  The coordinates (whether angles or Q-values) of the result
;					are established by the first scan evaluated.  If multiple
;					scans with different Alpha, Beta or Dth are combined, the
;					coordinates will be incorrect.
;	ORIENTATION
;		Character input specifying the detector orientation.  Two possible
;		values, "HOR" (for horizontal) and "VER" (for vertical).  First letter
;		is sufficient.  Default is vertical.
;	/RAW
;		Switch.  If set, the data is read "raw", meaning the X and Y
;		coordinates are expressed in pixels, not angles or Q-values.
;
;		Note:	When reading directly from a TIFF file, the data is always raw.
;				When reading through a SPEC file, the default coordinates are
;				Q_xy and Q_z.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;	TILT
;		Optional detector tilt angle (couterclockwise) in degrees.  Valid for
;		angle and Q-xy coordinates, has no effect if /RAW is set.
;	TITLE
;		An optional title for the resulting plot/image.  If not given, a
;		default title is generated internally and can be returned through TITLE.
;	/DISPLAY
;		Switch.  If set explicitly to 0, no display is generated.  By default
;		it is considered to be 1 (i.e. display is generated).
;	/CMARK
;		Switch.  If set, the center of the frame is marked with a cross and a
;		vertical line passing through this cross.  Not active when 1D data is
;		displayed.
;
;		Note:	If CMARK=2 is used, the vertical line is omitted.
;	XY_REG
;		A two element vector specifying horizontal region for integration in
;		[min,max] order.  Optionally, a scalar specifying the lower limit of the
;		region (with the higher limit provided by the data).  If not given, the
;		region is the whole horizontal range present.
;	Z_REG
;		Same as XY_REG, for vertical region.
;
;		Note 1	:	XY_REG and Z_REG can be both specified, to define a
;					rectangular region of interest.
;		Note 2	:	The coordinates (pixel, angle or Q) used in the regions
;					depend on the settings of SCAN_PD_READ.
;	/XY_INT											|	Note:	One and only one
;		Switch.  Specifies integration over Qxy.	|			of these two
;	/Z_INT											|			keywords must
;		Switch.  Specifies integration over Qz. 	|			be set.
;	RESULT
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
;		Note:	/NORM is set by default in PD_SUM, it can be disabled by
;				using NORM = 0 in the function call.
; OUTPUTS:
;		Standard output is graphics only, either a 2D image or a plot of 1D
;		integrated data.  Optional output through the RESULT keyword.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the data that's being displayed to the screen.  If it is image
;		data, it is returned in the 3D array format of 4 pages: XY-coordinates,
;		Z-coordinates, data, errors.  If it is integrated 1D Data, it is being
;		returned in the standard 3 column format (coordinate, data, errors)
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward, uses SPEC_FILE_CHECK, SCAN_PD_CENCOO, SCAN_PD_FRAMES,
;		SCAN_PD_READ and SCAN_PD_SHOW.  Calls DEFAULT, ISNUM, RANGE_COMP,
;		STRPARSE_MM and WHWERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-NOV-2007 by Mati Meron.
;		Modified 10-APR-2008 by Mati Meron.  Internal changes.
;		Modified 25-APR-2008 by Mati Meron.  Internal changes.
;		Modified 20-MAY-2008 by Mati Meron.  Enhanced to multi-scan capability.
;		Modified 20-JUN-2008 by Mati Meron.  Internal changes.
;		Modified 5-APR-2010 by Mati Meron.  Added keyword DISPLAY.
;		Modified 20-NOV-2013 by Mati Meron.  Added keyword CMARK.
;		MOdified 10-SEP-2014 by Mati Meron.  Internal changes.
;-

	on_error, 1

	if n_elements(snum) gt 0 and (size(snum))[0] eq 0 then begin
		Spec_file_check, snum, par_const= ['In_rot','Out_rot','Det_th'], $
		/pildet, /warn, nsc= nsc, lis= lis
		for i = 0l, nsc-1 do begin
			tem = Scan_PD_read(lis[i],fnum,orient=ori,/norm,raw=raw,angles=ang,$
			tilt= til,title= dtit,_extra = _e)
			if i gt 0 then begin
				dat[2,*,*] = dat[2,*,*] + tem[2,*,*]
				dat[3,*,*] = sqrt(dat[3,*,*]^2 + tem[3,*,*]^2)
			endif else dat = tem
		endfor
		if nsc gt 1 then begin
			l = Strparse_mm(dtit,'_',tlis)
			tlis[l-1] = ' S# ' + Range_comp(lis)
			dtit = strjoin(tlis[0,l-2],'_') + tlis[l-1] + ' (SUM)'
		endif
	endif else if Isnum(snum) then dat = snum

	if keyword_set(cmr) then begin
		if nsc eq 1 then begin
			dum = Wherinstruct('pin',_e)
			if dum ge 0 then pnfl = _e.(dum) else pnfl = 0
			wfnum= Scan_PD_frames(snum,fnum,nframes=nfnum,/uni,_extra=_e)
			if nfnum gt 1 then message, $
			'Multiple frames, center unverified!', /con
			wcen = Scan_PD_cencoo(snum,wfnum[0],raw=raw,ang=ang,pin=pnfl)
		endif else message, 'Multiple scans, CMARK ignored!', /con
	endif

	tit = Default(tit,Default(dtit,''))
	Scan_PD_show, dat, raw= raw, angles= ang, title= tit, display= dsp, $
		xy_reg= xyr,z_reg= zr,xy_int= xyi,z_int= zi,cmark= cmr,cen= wcen, $
		result= res,_extra= _e

	return
end