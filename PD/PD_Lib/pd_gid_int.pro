Function PD_GID_int, snum, xy_reg = xyr, z_reg = zr, xy_int = xyi, z_int = zi, $
	ermult= erm, smooth = smo, xy_ran = wxy, z_ran = wz, title= tit, _extra= _e

;+
; NAME:
;		PD_GID_INT
; VERSION:
;		8.33
; PURPOSE:
;		Integration of patched PD data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = PD_GID_INT( SNUM, ..., [,keywords])
; INPUTS:
;	SNUM
;		A list of scan numbers, in any form recognizable by RANGE_PROC.  If more
;		than one scan is provided, PD_GID_READ will attempt patching in the
;		horizontal direction.
;		Alternatively, SNUM may already contain PD data in the form returned by
;		PD_GID_READ.  In such case it is used as is.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	XY_REG
;		A two element vector specifying Qxy region of interest in [min,max]
;		order.  Optionally, a scalar specifying the lower limit of the ROI (with
;		the higher limit provided by the data.  If not given, the region is
;		determined by the Qxy values present in the data.
;	Z_REG
;		Same as XY_REG, for Qz.
;
;		Note:  XY_REG and Z_REG can be both specified, to define a rectangular
;		region of interest.
;	/XY_INT											|	Note:	One and only one
;		Switch.  Specifies integration over Qxy.	|			of these two
;	/Z_INT											|			keywords must
;		Switch.  Specifies integration over Qz. 	|			be set.
;	ERMULT
;		Error multiplier, if given multiplies the errors column of the result.
;	SMOOTH
;		Integer scalar, the width for Bincoef (default) smoothing of the data.
;		Should be an odd integer, any other input is rounded upwards to the
;		nearest odd integer.  See the routine PEAK_SMOOTH for more details.
;
;		Note:	Only integrated 1D results may be smoothed.
;	XY_RAN
;		Optional output, see below.
;	Z_RAN
;		Optional output, see below.
;	TITLE
;		Optional output (and input) variable, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to PD_GID_READ.
;		Not to be used directly.
; OUTPUTS:
;		Returns the integration result in the standard format of [3,N] array,
;		where the first column contains the Q-values, the second - the
;		integrated data and the third - the statistical error.
; OPTIONAL OUTPUT PARAMETERS:
;	XY_RAN
;		Returns the Q_xy limits that were actually used in the integration.
;	Z_RAN
;		Same as XY_RAN, for Q_z.
;	TITLE
;		If provided, returns the same, else returns a character scalar, made of
;		the file name and the scan numbers, to be used as title for plots.  See
;		PD_GID_READ for details.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, calls PD_GID_READ.  Calls PEAK_SMOOTH from SPEC.  Also
;		calls DEFAULT, ISNUM, ONE_OF and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-NOV-2007 by Mati Meron as a minor modification (changes only
;		in the input part) of LD_INT.
;		Modified 10-MAY-2008 by Mati Meron.  Added keyword SMOOTH.
;		Modified 10-OCT-2014 by Mati Meron.  Added keyword ERRMULT.
;-

	on_error, 1

	ityp = One_of(xyi,zi) + 1
	if ityp gt 0 then begin
		if (size(snum))[0] eq 3 then wdat = snum else $
		wdat = PD_GID_read( snum, title= dtit, _extra= _e)
		if (size(wdat))[0] ne 3 then message, 'Invalid Data!'
		tit = Default(tit,Default(dtit,''))

		qxyran = [min(reform(wdat[0,*,0]),max=max),max]
		qzran = [min(reform(wdat[1,0,*]),max=max),max]

		case n_elements(xyr) of
			0	:	wxy = qxyran
			1	:	wxy = [xyr[0],qxyran[1]]
			2	:	wxy = 1.*xyr
			else:	wxy = 1.*xyr[0:1]
		endcase
		wxy[0] = wxy[0] > qxyran[0]
		wxy[1] = wxy[1] < qxyran[1]

		case n_elements(zr) of
			0	:	wz = qzran
			1	:	wz = [zr[0],qzran[1]]
			2	:	wz = 1.*zr
			else:	wz = 1.*zr[0:1]
		endcase
		wz[0] = wz[0] > qzran[0]
		wz[1] = wz[1] < qzran[1]

		dum= where(wdat[0,*,0] ge wxy[0] and wdat[0,*,0] le wxy[1],ndum)
		if ndum gt 0 then wdat = wdat[*,dum,*] else message, 'Bad XY range!'
		dum = where(wdat[1,0,*] ge wz[0] and wdat[1,0,*] le wz[1], ndum)
		if ndum gt 0 then wdat = wdat[*,*,dum] else message,  'Bad Z range!'

		siz = size(wdat)

		res = make_array(3,siz[4-ityp],typ=Type(wdat))
		case ityp of
			1	:	res[0,*] = reform(wdat[1,0,*])
			2	:	res[0,*] = reform(wdat[0,*,0])
		endcase
		res[1,*] = total(reform(wdat[2,*,*]),ityp)
		res[2,*] = Default(erm,1.)*sqrt(total(reform(wdat[3,*,*])^2,ityp))
		if Isnum(smo) then res = Peak_smooth(res,wid=smo,_extra=_e)
	endif else message, 'Integration type not specified!'

	return, res
end