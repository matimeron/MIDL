Function Img_int, dat, xy_reg = xyr, z_reg = zr, xy_int = xyi, z_int = zi,$
	image = img, smooth = smo, indvar = ivr, emode = emd, nfactor = nfc, $
	xy_ran= wxy, z_ran= wz, ityp= ityp, _extra = _e

;+
; NAME:
;		IMG_INT
; VERSION:
;		8.02
; PURPOSE:
;		Integrates over AD data.
; CATEGORY:
;		X-ray utility.
; CALLING SEQUENCE:
;		Result = IMG_INT( DAT [, keywords])
; INPUTS:
;	DAT
;		Image data, a [4,M,N] array, optionally a 'just image" [M,N] array.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
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
;					depend on the settings of the routine providing the data.
;	/XY_INT											|	Note:	One and only one
;		Switch.  Specifies integration over Qxy.	|			of these two
;	/Z_INT											|			keywords must
;		Switch.  Specifies integration over Qz. 	|			be set.
;	/IMAGE
;		Switch.  Normally a 2D array is considered integrated data if the first
;		dimension is <=3.  If /IMAGE is set, it is taken as non-integrated and
;		is integrated accordingly.  Has no effect when DAT is a full 3D array.
;	SMOOTH
;		Integer scalar, the width for Bincoef (default) smoothing of the data.
;		Should be an odd integer, any other input is rounded upwards to the
;		nearest odd integer.  See the routine PEAK_SMOOTH for more details.
;
;		Note:	Only integrated 1D results may be smoothed.
;	INDVAR
;		The number of the Z-column (for XY integration) or XY-row (for Z
;		integration) to be picked as the independent variable of the result.
;		Default value is 0.
;	/EMODE
;		Switch.  If set, error is calculated for "product of Poissonians", i.e.
;		for the APEX detector, else "single Poissonian" square-root error is
;		calculated.  Active only for 2D non-integrated data.
;	XY_RAN
;		Optional output, see below.
;	Z_RAN
;		Optional output, see below.
;	ITYP
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to PEAK_SMOOTH.  Not to be used
;		directly.
; OUTPUTS:
;		Returns the input data, trimmed to fit the horizontal and/or vertical
;		integration regions, then (if so specified) integrated over the
;		horizontal or vertical coordinate.
;
;		Note that if an integration is performed, the output is of lower
;		dimensionality than the input.  Specifically, it is in the standard 1D
;		data format, i.e. [3,N] array.
;
;		In the special case when the input data is a 2D [M,N] array, the
;		integration and trimming coordinates are pixels.
; OPTIONAL OUTPUT PARAMETERS:
;	XY_RAN
;		Returns the horizontal limits that were actually used in the trimming.
;	Z_RAN
;		Same as XY_RAN, for vertical limits.
;	ITYP
;		Returns an integer scalar specifying the integration type, 1 for
;		horizontal, 2 for vertical (and 0 for none).
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None
; PROCEDURE:
;		Straightforward, calls PEAK_SMOOTH.  Calls ERREST from SURF_LIB.  Also
;		Calls DEFAULT, HOW_MANY and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 30-OCT-2007 by Mati Meron.
;		Modified 20-NOV-2007 by Mati Meron.  Merged the previous IMG_TRIM and
;		IMG_INT routines into a single routine.
;		Modified 10-APR-2008 by Mati Meron.  Added keyword INDVAR.
;		Modified 25-APR-2008 by Mati Meron.  Added option of a 2D array input.
;		Modified 5-MAY-2008 by Mati Meron.  Added keyword SMOOTH.
;		Modified 5-NOV-2009 by Mati Meron.  Added keyword IMAGE.  Some internal
;		changes.
;		Modified 25-MAR-2011 by Mati Meron.  Internal changes for non-standard
;		error calculations.
;-

	on_error, 1

	siz = size(dat)
	if siz[0] ge 3 then begin
		if How_many(fir=xyr,sec=zr) gt 0 then begin
			wdat = dat
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
		endif else wdat = dat

		if How_many(fir=xyi,sec=zi,/nozero,whi=ityp) gt 0 then begin
			if ityp lt 3 then begin
				siz = size(wdat)
				res = make_array(3,siz[4-ityp],typ=Type(wdat))
				ivr = Default(ivr,0l,/dtyp)
				case ityp of
					1	:	res[0,*] = reform(wdat[1,ivr,*])
					2	:	res[0,*] = reform(wdat[0,*,ivr])
				endcase
				res[1,*] = total(reform(wdat[2,*,*]),ityp)
				res[2,*] = sqrt(total(reform(wdat[3,*,*])^2,ityp))
				if Isnum(smo) then res = Peak_smooth(res,wid=smo,_extra=_e)
			endif else message, 'Only one dimension can be integrated!'
		endif else res = wdat
	endif else begin
		if siz[0] eq 2 then begin
			if siz[1] gt 3 or keyword_set(img) then begin
				if How_many(fir=xyr,sec=zr) gt 0 then begin
					pxyran = [0,siz[1]-1]
					pzran = [0,siz[2]-1]

					case n_elements(xyr) of
						0	:	wxy = pxyran
						1	:	wxy = floor([xyr[0],pxyran[1]])
						2	:	wxy = floor(xyr)
						else:	wxy = floor(xyr[0:1])
					endcase
					wxy = wxy[sort(wxy)]
					wxy[0] = wxy[0] > pxyran[0]
					wxy[1] = wxy[1] < pxyran[1]

					case n_elements(zr) of
						0	:	wz = pzran
						1	:	wz = floor([zr[0],pzran[1]])
						2	:	wz = floor(zr)
						else:	wz = floor(zr[0:1])
					endcase
					wz = wz[sort(wz)]
					wz[0] = wz[0] > pzran[0]
					wz[1] = wz[1] < pzran[1]

					wdat = dat[wxy[0]:wxy[1],wz[0]:wz[1]]
				endif else wdat = dat

				siz = size(wdat)
				if siz[0] eq 1 then begin
					wdat = reform(wdat,siz[1],1)
					siz = size(wdat)
				endif

				if How_many(fir=xyi,sec=zi,/nozero,whi=ityp) gt 0 then begin
					if ityp lt 3 then begin
						res = make_array(3,siz[3-ityp],typ=Type(wdat))
						res = reform(res,3,siz[3-ityp])
						ivr = Default(ivr,0l,/dtyp)
						res[0,*] = findgen(siz[3-ityp])
						res[1,*] = total(wdat,ityp)
						wnfc = Default(nfc,1.,/dtyp)
						if keyword_set(emd) then begin
							edat = Errest(wdat/wnfc,/emod,/squared)
							res[2,*] = wnfc*sqrt(total(edat,ityp)>0)						
						endif else res[2,*] = wnfc*sqrt((res[1,*]/wnfc)>0)
						if Isnum(smo) then $
						res = Peak_smooth(res,wid=smo,_extra=_e)
					endif else message, 'Only one dimension can be integrated!'
				endif else res = wdat
			endif else res = dat
		endif else message, 'Missing or invalid data!'
	endelse

	return, res
end