Pro PD_patch_z, sp_0, sp_1, sp_2, sp_3, sp_4, sp_5, sp_6, sp_7, $
	raw = raw, angles = ang, tilt = til, title = tit, lo_cut= lct, hi_cut= hct,$
	factors = mfac, result = res, _extra = _e

;+
; NAME:
;		PD_PATCH_Z
; VERSION:
;		7.15
; PURPOSE:
;		Patches together in the vertical direction horizontally integrated
;		Pilatus detector files
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_PATCH_Z, SNUM [, keywords])
; INPUTS:
;	SNUM
;		Scan number or a list of scan numbers, in any form acceptable by
;		RANGE_PROC.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/RAW
;		Switch.  If set, the data is read "raw", meaning the X and Y
;		coordinates are expressed in pixels, not angles or Q-values.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;	TILT
;		Optional detector tilt angle (couterclockwise) in degrees.  Valid for
;		angle and Q-xy coordinates, has no effect if /RAW is set.
;	TITLE
;		An optional title for the resulting plot/image.  If not given, a
;		default title is generated internally.
;	LO_CUT
;		Specifies the low cut of the data i.e. the level below which the data
;		should be cut.  For horizontal direction the low cut applies in the
;		horizontal dimension, and for vertical, in the vertical.  Given as
;		numbers in the [0,1] range, so that low cut of, say, 0.1, means "cut
;		data from 10% of the range and down".  The cut applies to raw data, 
;		before processing.
;
;		Note:	LO_CUT is never applied to the first scan on the list.
;	HI_CUT
;		Same as LOW_CUT, for the high end of the data.  Thus high cut of, say, 
;		0.8, means "cut data from 80% of the range and up".  The cut applies to
;		raw data, before processing.
;
;		Note:	HI_CUT is never applied to the last scan on the list.
;	FACTORS
;		Optional output, see below.
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
;		Note:	/NORM is set by default in PD_PATCH_Z, through PD_PATCH_GEN.
;				It can be disabled by using NORM = 0 in the function call.
; OUTPUTS:
;		Standard output is plot only.  Optional output through keywords.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the patched data in the standard [3,*] form:
;
;		Column 0	:	Q_z (or Beta if /ANGLES is set)
;		Column 1	:	intensity.
;		Column 2	:	errors.
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
;		None other that the data files must exist.
; PROCEDURE:
;		Standard patching using PD_PATCH_GEN.  Calls SCAN_PD_SHOW.  Calls
;		DEFAULT from MIDL.
; MODIFICATION HISTORY:
;		Created 10-NOV-2007 by Mati Meron.
;		Streamlined 15-NOV-2007 by Mati Meron.
;		Modified 10-APR-2008 by Mati Meron.  Internal changes.
;		Modified 15-APR-2009 by Mati meron.  Added keyword LO_CUT.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	res = PD_patch_gen(sp_0,sp_1,sp_2,sp_3,sp_4,sp_5,sp_6,sp_7, $
		ori= 'ver', raw= raw, angles= ang, tilt= til, lo_cut= lct, hi_cut= hct,$
		force= frc, slist= slis, factors= mfac, /noint, _extra= _e)

	dtit = fildat.name + '  Scan #:' + strcompress(strjoin(string(slis),', '))
	tit = Default(tit,dtit)

	Scan_PD_show, res, raw= raw, angles= ang, title= tit, /xy_int, _extra= _e

	return
end