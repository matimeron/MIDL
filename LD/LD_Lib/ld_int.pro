Function LD_int, sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, $
	xy_reg= xyr, z_reg= zr, xy_int= xyi, z_int= zi, xy_ran = wxy, z_ran = wz, $
	title = tit, _extra = _e

;+
; NAME:
;		LD_INT
; VERSION:
;		5.5
; PURPOSE:
;		Integration of linear detector data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = LD_INT( SL_0, ..., [,keywords])
; INPUTS:
;	SL_0, SL_1, SL_2, SL_3, SL_4, SL_5, SL_6, SL_7
;		List of linear detector scans, provided in any form that is acceptable
;		by LD_READ.  If more than one input is provided, LD_READ will
;		attempt patching in the horizontal direction.
;
;		Alternatively, SL_O may already contain linear detector data in the form
;		returned by LD_READ.  In such case it is used as is and any additional
;		inputs (if any) are ignored.
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
;	XY_RAN
;		Optional output, see below.
;	Z_RAN
;		Optional output, see below.
;	TITLE
;		Optional output (and input) variable, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to LD_READ.  Not
;		to be used directly.
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
;		LD_READ for details.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, calls LD_READ.  Also calls DEFAULT, ONE_OF and TYPE,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-FEB-2006 by Mati Meron.
;		Modified 20-JUN-2006 by Mati Meron.  Internal changes.
;-

	on_error, 1

	ityp = One_of(xyi,zi) + 1
	if ityp gt 0 then begin
	siz = size(sl_0)
		if siz[0] eq 3 then wdat = sl_0 else wdat = LD_read( $
		sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, title= dtit, _extra= _e)
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
		res[2,*] = sqrt(total(reform(wdat[3,*,*])^2,ityp))
	endif else message, 'Integration type not specified!'

	return, res
end