Pro Bimorph_show, dat

;+
; NAME:
;		BIMORPH_SHOW
; VERSION:
;		8.15
; PURPOSE:
;		Displays mirror scan data, highlighting segments.. 
; CATEGORY:
;		15ID Bimorph mirror specific.
; CALLING SEQUENCE:
;		BIMORPH_SHOW, DAT
; INPUTS:
;	DAT
;		Mirror scan data, in the format of the output of MIRROR_SLOPE.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
; 		Screen output only.  Plots the mirror scan data highlighting the mirror
; 		segments.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		BIMORPH_DAT.  See BIMORPH_INIT for details.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Straightforward.  Calls BIMORPH_INIT.  Calls SCAN_SHOW from SPEC.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;-

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	Bimorph_init

	xran = [bords[0], bords[nseg]]
	yran = [min(dat[1,*],max=max),max]
	bas = 10^floor(alog10(yran[1]-yran[0]))
	yran = bas*[floor(yran[0]/bas),ceil(yran[1]/bas)]
	Scan_show, dat, xran=xran, yran=yran, xsty=1, ysty=1, $
		xtit= 'mm', ytit= '!7l!xrad', tit= 'Mirror slope', _extra = _e
	find = lonarr(nseg)
	for i = 0, nseg-1 do begin
		dum = where(dat[0,*] ge bords[i] and dat[0,*] lt bords[i+1])
		find[i] = dum[0]
	endfor
	dum = where(find ge 0)
	ddat = dat[*,find]
	Scan_show, ddat, psym=3, /noerase,xran=xran, yran=yran, xsty=1, ysty=1, $
	/num, offnum=0.03
	cols = [!pcol.blue,!pcol.cyan]
	for i = 0, nseg-1 do begin
		dum = where(dat[0,*] ge bords[i] and dat[0,*] lt bords[i+1], ndum)
		if ndum gt 0 then oplot, dat[0,dum], dat[1,dum], col = cols[i mod 2], $
		psym = 8
	endfor

	return
end