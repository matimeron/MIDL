Function Unpri_sto, u, k, lamb, gamm, nper, name = nam

    on_error, 1

    u.k = Default(k,u.k)
    u.lamb = Default(lamb,u.lamb)
    u.gamm = Default(gamm,u.gamm)
    u.nper = Default(nper,u.nper)

    if Type(nam) eq 7 then begin
	wild = strpos(nam,'*')
	if wild gt 0 then nam = strmid(nam,0,wild) + '_k' + $
        string(floor(u.k),form = '(i2)') + 'p' + $
        string(round(100*(u.k - floor(u.k))), form = '(i02)')
	f = strcompress('(a' + string(strlen(u.name)) +')',/remove_all)
	u.name = string(strcompress(nam,/remove_all), format = f)
    endif

    return, u
end
