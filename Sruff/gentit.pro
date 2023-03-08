Pro Gentit, u, cur, dist, band, quantity = quan, total = tot, peak = pea, $
    smearstring = smest, tiltstring = tilst, surface = sur, $
    title = tit, subtitle = subtit, xtit = xti, ytit = yti, ztit = zti

    on_error, 1

    posib = ['area', 'volume', 'flux']
    full = [' power density', ' vol. power density', ' photon flux']
    tfull = ['Total power =', 'Total power =', 'Total flux =']
    pfull = ['Peak power density =', 'Peak vol. power density =', $
	    'Peak flux =']
    units = ['Watt/mm!E2!N', 'Watt/mm!E3!N', 'ph/s/mm!E2!N/']
    tunits = [' Watt', ' Watt', ' ph/s/']

    whi = Strmatch_mm(quan,posib,3)
    if whi eq -1 then message, 'Never heard about ' + quan
    smest = Default(smest,'Zero emittance',/strict)
    tilst = Default(tilst,'Normal incidence',/strict)

    if whi eq 2 then begin
	units[2] = $
	units[2] + strcompress(string(band,form="(g6.1,'bw')"),/remove_all)
	tunits[2] = $
	tunits[2] + strcompress(string(band,form="(g6.1,'bw')"),/remove_all)
    endif

    tit = strarr(2)
    subtit = strarr(2)
    xti = 'X (mm)'
    yti = 'Y (mm)'
    zti = full(whi) + ' (' + units(whi) + ')'

    tem = byte(strcompress(u.name,/remove_all))
    if tem[0] ge 96 then tem[0] = tem[0] - 32
    if keyword_set(sur) then tit[0]= string(tem) else tit[0]= string(tem) + zti
    tit[0] = tit[0] + ' ; K =' + strcompress(string(u.k, form = '(f7.3)')) + $
    ' ; I =' + strcompress(string(1000*cur, form="(i4,'mA')"))
    tit[1] = '!Cdist. =' + strcompress(string(dist, form = "(f7.3, 'm')")) + ' ; '
    subtit[0] = tfull(whi) + strcompress(string(tot,form= '(g10.4)')) + $
	tunits(whi) + ';  ' + pfull(whi) + $
	strcompress(string(pea,form= '(g10.4)')) + ' ' + units(whi) + '.'
    subtit[1] = '!C' + tilst + ';  ' + smest

    return
end
