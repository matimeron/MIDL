Function IPC_fun, qang, hq= hq, vq= vq, radius= rad, hlength= hln, ikern= ike, $
	_extra = _e

;+
; NAME:
;		IPC_FUN
; VERSION:
;		8.47
; PURPOSE:
;		Calculates the scattering form factor for an arbitrary in-plane 
;		orientation cylinder.
; CATEGORY:
;		Scattering specific.
; CALLING SEQUENCE:
;		Result = IPC_FUN ( QANG, HQ = HQ, VQ = VQ, RADIUS = RAD, HLENGTH = HLN,
;							[, IKERN = IKE])
; INPUTS:
;	QANG
;		Scalar or vector, the angle(s) between the cylinder axis and the
;		horizontal part of the Q-vector.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	HQ
;		Value(s) of the in-plane part of the Q-vector(s).
;	HV
;		The normal component of the Q-vector(s).
;	RADIUS
;		The cylinder radius, scalar.
;	HLENGTH
;		The cylinder half-length, scalar.
;	IKERN
;		Switch.  Set for "integration mode".
;
;		Note:
;			There are two distinct modes of operation, namely:
;			1)	Standard mode - QANG, HQ and VQ may be scalars or arrays of
;				arbitrary, but same dimensions.
;			2)	Integration mode - QANG may be arbitrary, but HQ and VQ have to
;				be scalars.
;	_EXTRA
;		Formal keyword used to transfer keywords from ROMBERG to IPC_FUN.  Not
;		to be used directly.
; OUTPUTS:
;		Returns the value(s) of the in-plane cylinder form factor for the
;		specified parameters.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the dimensional restrictions mentioned above.
; PROCEDURE:
; 		Straightforward.  Calls ARREQ, BINC, CODIMS, FPU_FIX and SP_BESELJ,
; 		from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2016 by Mati Meron.
;-

	on_error, 1

	if Codims(qang,hq,vq) then begin
		if keyword_set(ike) then $
		if not Arreq([size(hq,/dim),size(vq,/dim)],[0,0]) $
		then message, 'HQ and VQ need to be scalars for integration!'
		res = (SP_beselj(hq*cos(qang)*hln,0)* $
		Binc(sqrt(vq^2+hq^2*sin(qang)^2)*rad,1))^2
	endif

	return, FPU_fix(res)
end