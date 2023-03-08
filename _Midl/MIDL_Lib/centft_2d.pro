Function Centft_2D, img, x, y, delta = del, zero_mean = zem, detrend = det, $
	inverse= inv, integral= int, square_abs= sqa, pack_result= pac, show= sho, $
	kxy = kxy, _extra = _e

;+
; NAME:
;		CENTFT_2D
; VERSION:
;		7.09
; PURPOSE:
;		Interface to 2D FFT.
; CATEGORY:
;		Mathematical, general
; CALLING SEQUENCE:
;		Result = CENTFT_2D( IMG [, X, Y] [, keywords])
; INPUTS:
;	IMG
;		A two dimensional numeric array.  Mandatory.
;		Optionally, a 3D array may be used.  In such case, IMG[2,*,*] is taken
;		to be the actual image, while IMG[0,*,*] and IMG[1,*,*] are taken to be
;		X and Y (see below), respectively.
; OPTIONAL INPUT PARAMETERS:
;	X
;		An optional vector or 2D array of X values for the image.  If not
;		provided, an internal vector (containing consecutive pixel numbers)
;		is generated.
;
;		Optionally, X may be given as a [2,*,*] array.  In such case, X[0,*,*]
;		is used as X and X[1,*,*] as Y, internally.
;	Y
;		An optional vector or 2D array of Y values for the image.  If not
;		provided, an internal vector (containing consecutive pixel numbers) is
;		generated.
; KEYWORD PARAMETERS:
;	DELTA
;		Numeric scalar or 2-element vector, specifies the spacing of the X and
;		Y values if those are not given (ignored if X and Y are given).  If
;		provided as scalar, same value is used for X and Y.  Defaults to [1,1].
;	/ZERO_MEAN
;		Switch, specifies that the mean of IMG is to be subtracted from IMG
;		prior to transforming.  This is equivalent to setting the zero-order
;		value to zero after the transformation.
;	/DETREND
;		Switch.  When set in conjuction with /ZERO_MEAN the array is transformed
;		so that not only the global mean but the mean of any row/column is zero.
;	/INVERSE
;		Switch,  Same meaning as in FFT, signifies that an inverse transform
;		is to be taken.
;	/INTEGRAL
;		Switch.  If set, the Fourier transform is normalized according to the
;		Fourier integral convention.  This means multiplying by DX*DY for direct
;		transform and by DKX*DKY/(2*PI)^2 for inverse one.  The standard
;		normalization is multiplying by 1/(NX*NY) (where NX, NY, are the numbers
;		of points in the X and Y directions) for direct and by 1 for inverse.
;	/SQUARE_ABS
;		Switch.  If set, the square of the absolute value of the transform is
;		returned, instead of the transform itself.
;	/PACK_RESULT
;		Switch.  If set, the resulting transform and the KX and KY values
;		are packed together in a [3,NX,NY] array in the standard 2D data format.
;	/SHOW
;		Switch, if set an image of the output is displayed to the screen.
;	KXY
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to embedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the 2D Fourier transform of IMG, arranged so that the values
;		corresponding to zero frequency are in the center, instead of at the
;		boundaries.  The data type of the result is Complex (Double complex when
;		IMG is of type Double) unless /SQUARE_ABS is set, in which case the
;		result is of type Float (or Double).
;
;		Note:	Standard output is a 2D array of same dimensions as IMG.
;		However, when /PACK is set, the output is a 3D array of dimensions
;		[3,NX,NY]  such that Result[0,*,*] contains the KX values, Result[1,*,*]
;		contains the KY values and Result[2,*,*] contains the transform.
; OPTIONAL OUTPUT PARAMETERS:
;	KXY
;		Returns a [2,NX,NY] array contining the KX and KY values (frequencies).
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Applies FFT in combination with apropriate shifts to
;		get the required representation.
;		CENTFT_2D calls ABS_MM, CAST, DEFAULT, DISPLAY_MM, IMG_ZERO_MEAN, ISNUM
;		and REAL_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-MAY-2009 by Mati Meron.
;-

	on_error, 1

	wimg = reform(img)
	siz = size(wimg)
	dbtyp = Isnum(wimg,/double)
	del = Default(del,[1d,1d],/dtyp)
	if n_elements(del) eq 1 then del = [del,del]

	case siz[0] of
		2	:	begin
					sizx = (size(x))[0]
					if sizx eq 3 then begin
						wx = reform(x[0,*,0])
						wy = reform(x[1,0,*])
					endif else begin
						sizy = (size(y))[0]
						if sizx eq 2 then wx = reform(x[*,0]) $
						else if sizx eq 1 then wx = x
						if sizy eq 2 then wy = reform(y[0,*]) $
						else if sizy eq 1 then wy = y
					endelse
				end
		3	:	begin
					wx = reform(wimg[0,*,0])
					wy = reform(wimg[1,0,*])
					wimg = reform(wimg[2,*,*])
					siz = size(wimg)
				end
		else:	message, 'Not an image!'
	endcase
	wimg = Cast(wimg,5)
	if keyword_set(zem) then wimg = Img_zero_mean(wimg,det=det)
	if n_elements(wx) ge 2 then dx = Cast(wx[1] - wx[0],5) else dx = del[0]
	if n_elements(wy) ge 2 then dy = Cast(wy[1] - wy[0],5) else dy = del[1]

	sqfl = keyword_set(sqa)
	if keyword_set(inv) then idir = 1 else idir = -1

	nxy = siz[1:2]
	phn = dcomplex(0,-!dpi*idir/nxy)
	nhxy = (nxy-1)/2
	if nxy[0] mod 2 then begin
		vx = replicate(1d,nxy[0])
		xmult = 1d
	endif else begin
		vx = shift(exp(phn[0]*(lindgen(nxy[0]) - nhxy[0])),-nhxy[0])
		xmult = exp(-phn[0]/2)
	endelse
	if nxy[1] mod 2 then begin
		vy = replicate(1d,nxy[1])
		ymult = 1d
	endif else begin
		vy = shift(exp(phn[1]*(lindgen(nxy[1]) - nhxy[1])),-nhxy[1])
		ymult = exp(-phn[1]/2)
	endelse
	v = vx#transpose(vy)
	mult = xmult*ymult
	res = mult*shift(v*fft(v*shift(wimg,-nhxy),idir),nhxy)

	if keyword_set(int) then begin
		if keyword_set(inv) then imult = dx*dy/(2*!dpi)^2 $
		else imult = nxy[0]*nxy[1]*dx*dy
		res = imult*res
	endif
	if sqfl then res = Abs_mm(res)^2

	pacfl = keyword_set(pac)
	if pacfl or arg_present(kxy) then begin
		kxy = make_array([2,nxy],typ=4+dbtyp)
		kx = 2*!dpi/(nxy[0]*dx)*(lindgen(nxy[0]) - nhxy[0])
		for i = 0l, nxy[1] - 1 do kxy[0,*,i] = kx
		ky = 2*!dpi/(nxy[1]*dx)*(lindgen(nxy[1]) - nhxy[1])
		for i = 0l, nxy[0] - 1 do kxy[1,i,*] = ky
		if pacfl then begin
			pres = res
			res = make_array([3,nxy],typ=9)
			res[0:1,*,*] = kxy
			res[2,*,*] = pres
		endif
	endif
	rtyp = 3 +(1 + dbtyp)*(3 - 2*sqfl)

	if keyword_set(sho) then Display_mm, Real_mm(res), _extra = _e

	return, Cast(res,4,rtyp,/fix)
end