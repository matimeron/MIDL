Function Surf_thru, e, chi, epsx = epsx, epsy = epsy, dphi = dphi, $
	correct_at = rchi, sigx = sigx, sigy = sigy, radians = rad, degrees = deg,$
	fcryst = fcr, scryst = scr, find = find, sind = sind, $
	amp_corr = amc, absolute_flux = abf

;+
; NAME:
;		SURF_THRU
; VERSION:
;		4.3
; PURPOSE:
;		Calculating throughput through a 2-crystal system.
; CATEGORY:
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;
; MODIFICATION HISTORY:
;		Created by Mati Meron.
;-

	common mono_stuff, nfl, conv, cer, asftab

	on_error, 1

	if (One_of(deg,rad) > 0) eq 0 then amult = !dtor else amult = 1.
	wchi = amult*chi

	wepsx = amult*Default(epsx,0)
	wepsy = amult*Default(epsy,0)
	if n_elements(rchi) ne 0 then begin
		wrchi = amult*rchi
		wdphi = -(wepsx*cos(wrchi) - wepsy*sin(wrchi))
	endif else wdphi = amult*Default(dphi,0)

	sigx = Default(sigx,12e-6)
	sigy = Default(sigy,3e-6)

	fcr = Default(fcr,'diamond')
	scr = Default(scr,'silicon')

	find = Default(find,[1,1,1])
	sind = Default(sind,[1,1,1])

	fang = Bragg_angle(ener=e,crys=fcr,ind=find,/rad,dar=fdar,eta=feta)
	sang = Bragg_angle(ener=e,crys=scr,ind=sind,/rad,dar=sdar,eta=seta)

	famp = Ref_curve(0,ener=e,crys=fcr,ind=find)
	samp = Ref_curve(0,ener=e,crys=scr,ind=sind)

	ampfac = 1.
	if keyword_set(abf) then ampfac = feta/1e-3*famp*ampfac
	if keyword_set(amc) then ampfac = samp*ampfac

	a = feta/seta
	b = sqrt((sigx*cos(wchi))^2 + $
		(sigy*(sin(wchi) - tan(sang)/tan(fang)))^2)/sdar
	c = (wdphi + wepsx*cos(wchi) - wepsy*sin(wchi))/sdar

	return, ampfac*Mono_over(a,b,c)
end