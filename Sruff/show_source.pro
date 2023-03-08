Pro Show_source, usnam, arc = arc, wiggler = wig, undulator = und
;+
; NAME:
;       SHOW_SOURCE
; PURPOSE:
;       Provides information about sources in the list SORLIST.
; CATEGORY:
;       X-ray calculations.
; CALLING SEQUENCE:
;	SHOW_SOURCE [, USNAM] [, keywords]
; INPUTS:
;    USNAM
;       Name of a source.  If provided, the name must include either one or two
;	words, separated by comma, tab or space, for example 'NSLS, X17'.  If
;	only machine name is provided all sources on this machine will be
;	looked up.  If USNAM is not given, all the sources will be looked up.
; OPTIONAL INPUT PARAMETERS:
;       None.
; KEYWORD PARAMETERS:
;    ARC
;       Switch.  Specifies that only arc sources are to be considered.
;    WIGGLER
;       Switch.  Specifies that only wigglers are to be considered.
;    UNDULATOR
;       Switch.  Specifies that only undulators are to be considered.
; OUTPUTS:
;       None.
; OPTIONAL OUTPUT PARAMETERS:
;       None.
; COMMON BLOCKS:
;       SXR_STUFF.
; SIDE EFFECTS:
;       None.
; RESTRICTIONS:
;       SHOW_SOURCE can be used only after the source tables are loaded,
;       through a call to LOAD_SOURCES.
;	No more than one of the keyword parameters may be specified at the same
;	time.
; PROCEDURE:
;	If USNAM not provided, displayes names of all the sources on list that
;	fit the search criteria.  If only machine name is provided, displayes
;	names of the fitting sources on this machine.  If both machine and
;	source name are given, displayes full source information.
;	Calling ONE_OF, STRPARSE_MM and STRMATCH_MM from MIDDLE.
; MODIFICATION HISTORY:
;       Created 25-MARCH-1993 by Mati Meron.
;-

    common sxr_stuff, sorlist, abctab, enun, denun, curbeam

    on_error, 1
    stypes = ['','Arc', 'Wiggler', 'Undulator']

    who = Strparse_mm(usnam,'	, ',par)
    if who le 0 then begin
	what = One_of(arc,wig,und)
	if who eq 0 then begin
	    nso = Strmatch_mm(par(0), sorlist.machine)
	    if nso ge 0 then soran = [nso,nso] else $
	    message, 'Machine ' + par(0) + ' not on list!'
	endif else soran = [0, n_elements(sorlist) - 1]
	for nso = soran(0), soran(1) do begin
	    go = 1
	    if what eq -1 then begin
		ndo = sorlist(nso).nlines + 1
		liran = indgen(ndo)
	    endif else liran = where(sorlist(nso).line.type eq what + 1, ndo)
	    if ndo gt 0 then begin
		print
		print, sorlist(nso).machine, ':'
		for nli = 0, ndo - 1 do begin
		    print, sorlist(nso).line(liran(nli)).name, $
		    stypes(sorlist(nso).line(liran(nli)).type), $
		    format = '(t12,a,t24,a)'
		endfor
	    endif
	endfor
    endif else begin
	nso = Strmatch_mm(par(0), sorlist.machine)
	if nso eq -1 then message, 'Machine ' + par(0) + ' not on list!'
	nli = Strmatch_mm(par(1), sorlist(nso).line(0:sorlist(nso).nlines).name)
	if nli eq -1 then message, $
	'Device '+par(1)+' not listed for '+ par(0)+ '!'
	print
	print, sorlist(nso).machine, sorlist(nso).line(nli).name, $
	stypes(sorlist(nso).line(nli).type), format = '(a,t12,a,t24,a)
	print
	print, 'Energy		= ', sorlist(nso).energy, ' (GeV)'
	print, 'Current		= ', 1e3*sorlist(nso).current, ' (mA)'
	print, 'Critical Energy	= ', 1e-3*sorlist(nso).line(nli).ecrit, ' (keV)'
	print
	print, 'Hor. spread	= ', sorlist(nso).line(nli).mrad, ' (mrad)'
	print, 'Sigma_x		= ', sorlist(nso).line(nli).sigx, ' (mm)'
	print, 'Sigma_y		= ', sorlist(nso).line(nli).sigy, ' (mrad)'
	print
	if sorlist(nso).line(nli).type gt 1 then begin
	    print, 'K value	= ', sorlist(nso).line(nli).kval
	    print, '# of poles	= ', sorlist(nso).line(nli).poles
	print
	endif
    endelse

    return
end
