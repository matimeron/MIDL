Function ABC_gen, ene, rsig= rsg, asig= asg, bandwidth= ban, amplitude= amp, $
	_extra = _e

;+
; NAME:
;		ABC_GEN
; VERSION:
;		8.72
; PURPOSE:
;		Generates an {ABC} type structure.
; CATEGORY:
;		Optics ABC-formalism calculations.
; CALLING SEQUENCE:
;		Result = ABC_GEN ( ENE, [, keywords])
; INPUTS:
;	ENE
;		Scalar, beam energy.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RSIG
;		Beam spatial sigma, in mm.  Must be given as a 2-element vector, in
;		[horizontal, vertical] order.  If not given, default values will be read
;		from the global parameter !BLPAR.
;	ASIG
;		Same as RSIg, for angular sigma, value(s) in mr.
;
;		Note:	While the default units for RSIG and ASIG are mm and mr,
;				respectively, ABC_GEN will recognize, within reasonable limits,
;				inputs provided in meter and radian, or micrometer and
;				microradian units, and will convert to the default units as
;				needed.
;	BANDWIDTH
;		Scalar, the bandwidth of the beam.  The default value is 0.001.
;	AMPLITUDE
;		Scalar, the amplitude of the ABC beam.  The default value is calculated
;		from RSIG, ASIG amd AMPLITUDE so as to yield integral = 1.
;	_EXTRA
;		A formal keyword for transfering keywords to embedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Returns an ABC structure, populated with values derived from RSIG, ASIG,
;		BANDWIDTH and AMPLITUDE, or their defaults.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, from definitions.  Calls ARREQ, DEFAULT, and DIAGOARR,
;		from MIDL, and UND_BEAMSIZE from SRUFF.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;		Modified 20-DEC-2020 by Mati Meron.  Internal changes.
;-

	on_error, 1

	if n_elements(ene) eq 1 then $
	drsg = Und_beamsize(ene,/def,dis=0,ang=dasg,_extra=_e) $
	else message, 'Scalar energy required!'
	wrsg = Default(rsg,drsg,lo=5)
	wasg = Default(asg,dasg,lo=5)
	if not Arreq([n_elements(wrsg),n_elements(wasg)],[2,2]) $
	then message, 'RSIG and ASIG need 2 elements each!'
	sfac = 10d^(3-3*total(max(wasg)*[1d,1d3] gt 1))
	wrsg = sfac*wrsg
	wasg = sfac*wasg
	wban = Default(ban,1d-3,lo=5)
	wamp = Default(amp,1d/((2*!dpi)^2*wban*product(wrsg)*product(wasg)),/dtyp)

	abc = {abc}
	abc.amp = wamp
	tem = [wrsg,wasg]^2
	diag = 0*tem
	dum = where(tem ne 0, ndum)
	if ndum gt 0 then diag[dum] = 1/tem[dum]
	abc.amat = Diagoarr(diag)
	abc.con2 = 2*!dpi/wban^2

	return, abc
end