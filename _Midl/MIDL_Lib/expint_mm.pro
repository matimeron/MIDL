Function Expint_mm, x, n, eps

;+
; NAME:
;		EXPINT_MM
; VERSION:
;		8.2
; PURPOSE:
;		Calculates the exponential integral function.  Replacement for the IDL
;		EXPINT function which accepts only real input.
; CATEGORY:
;		Mathematical function (general).
; CALLING SEQUENCE:
;		Result = EXPINT_MM (X, N [,EPS ])
; INPUTS:
;	X
;		Numeric, otherwise arbitrary.
;	N
;		Numeric scalar of integer type, real, non-negative.
; OPTIONAL INPUT PARAMETERS:
;	EPS
;		Specifies precision level.  Default is machine precision.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
;		Returns the exponential integral function of x.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		1)	N must be >=0.
; 		2)	The real part of X must be >=0.
; 		3)	For N = 0, 1, the real part of X must be >0.
; PROCEDURE:
;		Uses series expansion for small ABS(X) and continued fraction
;		expansion for larger values.  Calls ABS_MM, CAST, CONFRAC, DEFAULT,
;		DIGAMMA_MM, ISNUM, REAL_MM, TOLER and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JAN-2013 by Mati Meron as a variation of IGAMMA_MM.
;-

	on_error, 1
	weps = Default(eps,Toler(x),/dtype)

	if min(Real_mm(x)) lt 0 then message, 'Real part of X must be >= 0!'

	if Isnum(n,/int) then begin
		if n ge 0 then begin
			wx = dcomplex(x)
			res = 0*wx
			if n gt 0 then begin
				dum = where(wx eq 0, ndum)
				if ndum gt 0 then begin
					if n gt 1 then res[dum] = 1d/(n-1) $
					else message, 'X = 0 not allowed for N < 2!'
				endif
				dum = where(Abs_mm(wx) gt 0 and Abs_mm(wx) le 1, ldum)
				if ldum gt 0 then begin
					tx = wx[dum]
					tres = res[dum]
					tem = tres + 1
					ltem = lindgen(ldum)
					m = 0l
					while ldum gt 0 do begin
						if m ne (n-1) then begin 
							term = tem/(m-n+1)
							tres[ltem] = tres[ltem] - term
;							stop
							eest = Abs_mm(term*tx[ltem])/ $
								(Abs_mm(tx[ltem]+m+1) > weps)
							lltem = where(eest gt weps*Abs_mm(tres[ltem]),ldum)
						endif else begin
							term = tem*(alog(tx[ltem])-Digamma_mm(n))
							tres[ltem] = tres[ltem] - term
							lltem = ltem
						endelse
						if ldum gt 0 then begin
							m = m + 1
							tem = (-tem*tx[ltem])[lltem]/m
							ltem = ltem[lltem]
						endif
					endwhile
					res[dum] = tres
				endif
				dum = where(Abs_mm(wx) gt 1, ldum)
				if ldum gt 0 then begin
					len = ceil (-alog(weps)*(4/sqrt(n) > sqrt(n)/4))
					alist = double([1l,1 + lindgen(len)/2])
					tlist = 1l + 2*lindgen(len/2)
					alist(tlist) = alist(tlist) + n - 1
					blist = dblarr(len + 1, 2)
					blist(tlist,0) = 1
					blist([0, tlist + 1],1) = 1
					tx = wx[dum]
					tres = exp(-tx)*Confrac(alist,blist,tx,eps = weps, /rel)
					res[dum] = tres
				endif
			endif else res = exp(-x)/x
		endif else message, 'N must be non-negative!'
	endif else message, 'N must be of integer type!'

	return, Cast(res,4,Type(x),/fix)
end