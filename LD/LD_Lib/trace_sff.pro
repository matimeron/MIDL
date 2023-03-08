Function Trace_sff, rad, dat

;+
; NAME:
;		TRACE_SFF
; VERSION:
;		5.2
; PURPOSE:
;		Adding form factor data to a scattering image.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = TRACE_SFF (RAD, DAT)
; INPUTS:
;	RAD
;		Numeric scalar, the radius of a spherical scatterer.
;	DAT
;		Image data represented in a 3D format as
;			DAT[0,*,*] = QXY coordinates of the image.
;			DAT[1,*,*] = QZ coordinates of the image.
;			DAT[2,*,*] = The image itself.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the input data with the pattern of the zero curves of a
;		spherical scattering factor superimposed on it.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.
; MODIFICATION HISTORY:
;		Created 5-DEC-2005 by Mati Meron.
;-

	on_error, 1

	res = dat
	if rad gt 0 then begin
		qxy = reform(dat[0,*,*])
		qz = reform(dat[1,*,*])
		vqxy = reform(qxy[*,0])
		vqz = reform(qz[0,*])
		dqxy = (max(vqxy,min=min) - min)/(n_elements(vqxy) - 1)
		dqz = (max(vqz,min=min) - min)/(n_elements(vqz) - 1)
		del = sqrt(dqxy^2 + dqz^2)/2
		qmax = sqrt(max(abs(vqxy))^2 + max(abs(vqz))^2)
		qmin = sqrt(min(abs(vqxy))^2 + min(abs(vqz))^2)

		nmax = floor(qmax*rad/!pi - 0.5)
		if nmax gt 0 then begin
			nn = lindgen(nmax+1)
			val = where(nn ge (ceil(qmin*rad/!pi - 0.5) > 1), nval)
			if nval gt 0 then begin
				nn = nn[val]
				arn = (nn + 0.5)*!pi
				qnn = (arn - 2/(arn + sqrt(arn^2-8./3)))/rad
				q = sqrt(qxy^2 + qz^2)
				img = reform(dat[2,*,*])
				amp = max(img)
				for i = 0, nval - 1 do begin
					dum = where(abs(q - qnn[i]) le del, ndum)
					if ndum gt 0 then img[dum] = amp
				endfor
				res[2,*,*] = img
			endif
		endif
	endif

	return, res
end





