Function Osc_fun, p, q, grad = grd, hess = hes

;+
; NAME:
;		OSC_FUN
; VERSION:
;		8.42
; PURPOSE:
;		Evaluates a function consisting of a sum of constant, declining exponent
;		and an arbitrary number of sinusoids.  The function's purpose is to 
;		serve as evaluation function for fitting, in OSC_FIT.
; CATEGORY:
;		Liquid compression specific.
; CALLING SEQUENCE:
;		Result = OSC_FUN(P, Q [, keywords])
; INPUTS:
;	P
;		Numeric vector, containing the parameters of the function.  The length
;		of the vector must be an even number >=6.  The parameters, in order, are
;			P[0]:	Constant.
;			P[1]:	Exponent amplitude.
;			P[2]:	Exponent factor (the exponential function is 
;					P[1]*exp(-P[2]*Q).
;			P[3]:	Base angular frequency.
;			P[4]:	First Sine amplitude.
;			P[5]:	First Sine phase.
;
;		Additional values, if present, are pairs of amplitudes and phases for
;		sines with angular frequency being an integer multiple of P[3]
;	Q
;		Numeric vector.  Serves as the X values for the generated function.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GRAD
;		Optional output, see below.
;	HESS
;		Optional output, see below.
; OUTPUTS:
;		Returns the evaluated function as a the sum:
;		P[0] + P[1]*exp(-P[2]*Q) + P[4]*sin(P[3]*Q + P[5]) + 
;		[ P[6]*sin(2*P[3]*Q + P[7]) + ...]
;		The dimension of the result is the dimension of Q.
; OPTIONAL OUTPUT PARAMETERS:
;	GRAD
;		Returns the gradient of the result as a 2D matrix of dimension [Nq,Np]
;	HESS
;		Returns the Hessian (second derivative) of the result as a 3D array
;		of dimension [Nq,Np,Np]
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, evaluation from definitions.  Calls CALCTYPE and CAST,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2014 by Mati Meron.
;-

	if n_params() ne 2 then message, 'Wrong number of inputs!'
	typ = Calctype(p,q,0.)
	np = n_elements(p)
	if np lt 6 then message, 'Not enough parameters!'
	if np mod 2 ne 0 then message, 'even number of parameters required!'
	wp = Cast(p,typ)
	ns = (np - 4)/2
	nq = n_elements(q)
	wq = Cast(reform([q],nq),typ)

	fe = exp(-(wp[2]>0)*wq)
	res = wp[0] + wp[1]*fe
	if ns gt 0 then begin
		fs = (fc = make_array(nq,ns,typ=typ))
		i = lindgen(ns)
		j = 4 + 2*i
		k = j + 1
		m = i + 1
		for i = 0, ns-1 do begin
			fs[*,i] = sin(m[i]*wp[3]*wq + wp[k[i]])
			fc[*,i] = cos(m[i]*wp[3]*wq + wp[k[i]])
			res = res + wp[j[i]]*fs[*,i]
		endfor
	endif

	if arg_present(grd) then begin
		grd = make_array(nq,np,typ=typ)
		grd[*,0] = 1
		grd[*,1] = fe
		grd[*,2] = -wp[1]*wq*fe
		for i = 0, ns-1 do begin
			grd[*,3] = grd[*,3] + m[i]*wp[j[i]]*wq*fc[*,i]
			grd[*,j[i]] = fs[*,i]
			grd[*,k[i]] = wp[j[i]]*fc[*,i]
		endfor
		grd = Cast(grd,typ,typ,/fix)
	endif

	if arg_present(hes) then begin
		hes = make_array(nq,np,np,typ=typ)
		hes[*,1,2] = (hes[*,2,1] = -wq*fe)
		hes[*,2,2] = wp[1]*wq^2*fe
		for i = 0, ns-1 do begin
			hes[*,3,3] = hes[*,3,3] - m[i]^2*wp[j[i]]*wq^2*fs[*,i]
			hes[*,3,j[i]] = (hes[*,j[i],3] = m[i]*wq*fc[*,i])
			hes[*,3,k[i]] = (hes[*,k[i],3] = -m[i]*wp[j[i]]*wq*fs[*,i])
			hes[*,j[i],k[i]] = (hes[*,k[i],j[i]] = fc[*,i])
			hes[*,k[i],k[i]] = - wp[j[i]]*fs[*,i]
		endfor
		hes = Cast(hes,typ,typ,/fix)
	endif

	return, Cast(res,typ,typ,/fix)
end