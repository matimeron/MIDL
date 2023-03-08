Pro PR_info, prstruct = p

    on_error, 1

    typnams = ['Unknown', 'Abs. power', 'Flux']

    print
    print, 'type    	=  ', strtrim(typnams(p.type),2)
    print, 'current 	=  ', 1e3*p.cur, format = "(a,f5.1,' mA')"
    print, 'distance  	=  ', p.dist, format = "(a,f5.2,' m')"
    print, 'window     	=  ', p.win, format = "(a,' [ ',f5.2,', ',f5.2,' ] mm')"
    print
    if p.conv then print, 'Corrected for source emittance' else $
	print, 'Zero emittance'
    print
    if p.sin_ang ne 1 then begin
	tet = !radeg*asin(p.sin_ang)
	if p.xtilt then $
	print, 'X-tilt  	=  ', tet, format = "(a,f6.3,' deg.')" $
	else print, 'Y-tilt  	=  ', tet, format = "(a,f6.3,' deg.')"
    endif else print, 'Normal incidence'
    if p.type eq 1 then $
    print, 'absorber	=  ', p.absor else $
    if p.type eq 2 then $
    print, 'bandwidth   =  ', p.ban, format = '(e8.2)'
    if p.nfilts ne 0 then begin
	print
	for i = 0, p.nfilts -1 do $
	print, p.filts(i), ' filter, ', p.filths(i), format="(a,a,f6.3,' mm')"
    endif
    if p.nmirrs ne 0 then begin
	print
	for i = 0, p.nmirrs -1 do $
	print, p.mirrs(i), ' mirror at ',p.mirans(i),format="(a,a,f6.3,' mrad')"
    endif
    if not Streq(p.extra,' ',1) then begin
	print
	print, strtrim(p.extra) + ' applied to data,'
	if p.nparams gt 0 then $
	print, 'with the parameters: ', p.params(0:p.nparams-1)
    endif
    print

    return
end
