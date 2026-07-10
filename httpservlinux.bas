Rem Geek Basic Web Server
Rem http://www.geekbasic.com

_Title "Geek Basic Web Server"

Let crlf$ = Chr$(13) + Chr$(10) ' carrige return, line feed

Const users = 100 'total users allowed to connect at a time
Const totalfiles = 1000 'total files to be accounted for
Const vars = 100
Const lines = 1000

Dim userid(users) 'client handle
Dim filename$(totalfiles) 'a list of files will be loaded in here
Dim varname$(vars)
Dim varval(vars)
Dim strname$(vars)
Dim strval$(vars)
Dim cmd$(lines)
Dim par$(lines)

Input "Enter a port:"; port

GoSub checkfiles 'read in the list of files
GoSub starthosting 'start serving or exit on failure

main:

GoSub checkconnection 'assign handles to new connections
GoSub handlerequests 'check for and respond to input

If Inp(96) = 1 Then

    End

Else

    _Delay .01
    GoTo main

End If

starthosting:

serverhandle = _OpenHost("TCP/IP:" + Str$(port))

If serverhandle Then

    Print "Server started on port #"; port

Else

    Print "Unable to start server"
    Sleep
    End

End If

Return

checkfiles:

Shell _Hide "ls -1 www > files.dat"

Open "files.dat" For Input As #1

Let checkfile = 0

While Not EOF(1)

    Line Input #1, filename$(checkfile)
    Let checkfile = checkfile + 1

Wend

Close #1

Return

checkconnection:

Let newconnection = _OpenConnection(serverhandle)

If newconnection Then

    For user = 0 To users - 1

        If userid(user) = 0 Then

            Let userid(user) = newconnection
            Print "New connection handle:"; userid(user)

            Exit For

        End If

    Next user

End If

Return

handlerequests:

For user = 0 To users - 1

    If userid(user) Then

        Get #userid(user), , indata$

        Let indata$ = LTrim$(RTrim$(indata$))

        If indata$ <> "" Then

            If Left$(UCase$(indata$), 5) = "GET /" Then

                Let indata$ = Right$(indata$, Len(indata$) - 5)

                If InStr(UCase$(indata$), "HTTP/1.1") Then Let httptype$ = "HTTP/1.1"
                If InStr(UCase$(indata$), "HTTP/1.0") Then Let httptype$ = "HTTP/1.0"

                Let indata$ = Left$(indata$, InStr(indata$, " ") - 1)

                If indata$ = "" Or indata$ = "/" Then

                    Let indata$ = "index.html"

                End If

                Let page$ = ""

                GoSub decodeurl

                For checkfile = 0 To totalfiles

                    If checkfile = totalfiles Then

                        Let page$ = "<!DOCTYPE html><html><body><b>Error!</b><br />The page you requested doesn't exist.</body></html>"

                        Exit For

                    ElseIf UCase$(indata$) = UCase$(filename$(checkfile)) Then

                        If InStr(UCase$(filename$(checkfile)), ".PNG") Then

                            Open "www/" + filename$(checkfile) For Binary As #1

                        Else

                            Open "www/" + filename$(checkfile) For Input As #1

                        End If

                        For check = 0 To lines - 1

                            Let cmd$(check) = ""
                            Let par$(check) = ""

                        Next check

                        Let pointline = 0

                        While Not EOF(1)

                            If InStr(UCase$(filename$(checkfile)), ".HTML") Then

                                Line Input #1, indata$
                                Let indata$ = LTrim$(RTrim$(indata$)) + crlf$
                                Let page$ = page$ + indata$

                            ElseIf InStr(UCase$(filename$(checkfile)), ".GBWS") Then

                                Line Input #1, indata$
                                Let indata$ = LTrim$(RTrim$(indata$))

                                Let space = InStr(indata$, " ")

                                If space Then

                                    Let cmd$(pointline) = Left$(indata$, space - 1)
                                    Let par$(pointline) = Mid$(indata$, space + 1, Len(indata$) - space)

                                Else

                                    Let cmd$(pointline) = indata$
                                    Let par$(pointline) = ""

                                End If

                                If UCase$(cmd$(pointline)) = "END" Then

                                    GoSub executescript
                                    Exit While

                                End If

                                Let pointline = pointline + 1

                            Else

                                Get #1, , indata$
                                Let page$ = page$ + indata$

                            End If

                        Wend

                        Close #1

                        Exit For

                    End If

                Next checkfile

                If Right$(UCase$(filename$(checkfile)), 4) = ".PNG" Then

                    Let message$ = httptype$ + " 200 OK" + crlf$
                    Let message$ = message$ + "Content-Type: img/png" + crlf$
                    Let message$ = message$ + "Content-Length: " + Str$(Len(page$)) + crlf$
                    Let message$ = message$ + "Server: GeekBasicWebServer" + crlf$
                    Let message$ = message$ + "Date: " + Date$ + crlf$
                    Let message$ = message$ + "Conection: Keep-Alive" + crlf$
                    Let message$ = message$ + crlf$ + page$

                Else

                    Let message$ = httptype$ + " 100 Continue" + crlf$ + "" + crlf$
                    Let message$ = message$ + httptype$ + " 200 OK" + crlf$
                    Let message$ = message$ + "Content-Type: text/html" + crlf$
                    Let message$ = message$ + "Content-Length: " + Str$(Len(page$)) + crlf$
                    Let message$ = message$ + "Server: GeekBasicWebServer" + crlf$
                    Let message$ = message$ + "Date: " + Date$ + crlf$
                    Let message$ = message$ + crlf$ + page$

                End If

                Put #userid(user), , message$

            End If

            If Not EOF(userid(user)) Then Close userid(user)
            Let userid(user) = 0

        End If

    End If

Next user

Return

executescript:

For var = 0 To vars - 1

    Let varname$(var) = ""
    Let varval(var) = 0
    Let strname$(var) = ""
    Let strval$(var) = ""

Next var

For pointline = 0 To lines - 1

    Let c$ = cmd$(pointline)
    Let p$ = par$(pointline)

    Select Case UCase$(c$)

        Case "INTEGER": GoSub executeinteger
        Case "STRING": GoSub executestring
        Case "STRSET": GoSub executestrset
        Case "CONCAT": GoSub executeconcat
        Case "STRCMP": GoSub executestrcmp
        Case "STRVAL": GoSub executestrval
        Case "STRLEN": GoSub executestrlen '
        Case "STRFIND": GoSub executestrfind '
        Case "STRCUT": GoSub executestrcut '
        Case "STRTRIM": GoSub executestrtrim
        Case "STRUCASE": GoSub executestrucase
        Case "STRLCASE": GoSub executestrlcase
        Case "LET": GoSub executelet
        Case "IF": GoSub executeif
        Case "GOTO": GoSub executegoto
        Case "RANDOM": GoSub executerandom
        Case "OUTPUT": GoSub executeoutput
        Case "FORMGET": GoSub executeformget
        Case "FORMPOST": GoSub executeformpost '
        Case "DATE": GoSub executedate
        Case "TIME": GoSub executetime
        Case "NEWFILE": GoSub executenewfile
        Case "LOADFILE": GoSub executeloadfile
        Case "APPENDFILE": GoSub executeappendfile
        Case "CLOSEFILE": GoSub executeclosefile
        Case "GETSTRING": GoSub executegetstring
        Case "PUTSTRING": GoSub executeputstring
        Case "CHECKFILE": GoSub executecheckfile
        Case Else: GoSub checksyntax

    End Select

Next pointline

Return

executeinteger:

Let comma = InStr(p$, ",")
Let name$ = Left$(p$, comma - 1)
Let p$ = Right$(p$, Len(p$) - comma)

For check = 0 To vars - 1

    If varname$(check) = "" Then

        Let varname$(check) = name$
        Let varval(check) = Val(p$)
        Exit For

    End If

Next check

Return

executestring:

Let comma = InStr(p$, ",")
Let name$ = Left$(p$, comma - 1)
Let p$ = Right$(p$, Len(p$) - comma)

For check = 0 To vars - 1

    If strname$(check) = "" Then

        Let strname$(check) = name$
        Let strval$(check) = p$
        Exit For

    End If

Next check

Return

executestrset:

Let comma = InStr(p$, ",")
Let name$ = Left$(p$, comma - 1)
Let p$ = Right$(p$, Len(p$) - comma)

For check = 0 To vars - 1

    If UCase$(strname$(check)) = UCase$(name$) Then

        Let strval$(check) = p$
        Exit For

    End If

Next check

Return

executeconcat:

Let comma = InStr(p$, ",")
Let name$ = Left$(p$, comma - 1)
Let p$ = Right$(p$, Len(p$) - comma)

Let comma = InStr(p$, ",")
Let name2$ = Left$(p$, comma - 1)
Let p$ = Right$(p$, Len(p$) - comma)

If Left$(name2$, 1) <> "$" Then

    Let concatstr1$ = name2$

Else

    Let name2$ = Right$(name2$, Len(name2$) - 1)

    For check = 0 To vars - 1

        If UCase$(name2$) = UCase$(strname$(check)) Then

            Let concatstr1$ = strval$(check)

            Exit For

        End If

    Next check

End If

If Left$(p$, 1) <> "$" Then

    Let concatstr2$ = p$

Else

    Let p$ = Right$(p$, Len(p$) - 1)

    For check = 0 To vars - 1

        If UCase$(p$) = UCase$(strname$(check)) Then

            Let concatstr2$ = strval$(check)

            Exit For

        End If

    Next check

End If

For check = 0 To vars - 1

    If UCase$(name$) = UCase$(strname$(check)) Then

        Let strval$(check) = concatstr1$ + concatstr2$

        Exit For

    End If

Next check

Return

executestrcmp:

Let comma = InStr(p$, ",")
Let name$ = Left$(p$, comma - 1)
Let p$ = Right$(p$, Len(p$) - comma)

Let comma = InStr(p$, ",")
Let name2$ = Left$(p$, comma - 1)
Let p$ = Right$(p$, Len(p$) - comma)

For check = 0 To vars - 1

    If UCase$(strname$(check)) = UCase$(name$) Then

        For check2 = 0 To vars - 1

            If UCase$(strname$(check2)) = UCase$(name2$) Then

                If strval$(check) = strval$(check2) Then

                    For check3 = 0 To lines - 1

                        If UCase$(cmd$(check3)) = "LABEL" And UCase$(par$(check3)) = UCase$(p$) Then

                            Let pointline = check3

                            Exit For

                        End If

                    Next check3

                End If

                Exit For

            End If

        Next check2

        Exit For

    End If

Next check

Return

executestrval:

Let comma = InStr(p$, ",")
Let name$ = Left$(p$, comma - 1)
Let p$ = Right$(p$, Len(p$) - comma)

For check = 0 To vars - 1

    If UCase$(varname$(check)) = UCase$(name$) Then

        For check2 = 0 To vars - 1

            If UCase$(strname$(check2)) = UCase$(p$) Then

                Let varval(check) = Val(strval$(check2))

                Exit For

            End If

        Next check2

        Exit For

    End If

Next check

Return

executestrlen:

Return

executestrfind:

Return

executestrcut:

Return

executestrtrim:

For check = 0 To vars - 1

    If UCase$(strname$(check)) = UCase$(p$) Then

        Let strval$(check) = LTrim$(RTrim$(strval$(check)))
        Exit For

    End If

Next check

Return

executestrucase:

For check = 0 To vars - 1

    If UCase$(strname$(check)) = UCase$(p$) Then

        Let strval$(check) = UCase$(strval$(check))
        Exit For

    End If

Next check

Return

executestrlcase:

For check = 0 To vars - 1

    If UCase$(strname$(check)) = UCase$(p$) Then

        Let strval$(check) = LCase$(strval$(check))
        Exit For

    End If

Next check

Return

executelet:

Let op$ = ""

If InStr(p$, "+") Then Let op$ = "+"
If InStr(p$, "-") Then Let op$ = "-"
If InStr(p$, "*") Then Let op$ = "*"
If InStr(p$, "/") Then Let op$ = "/"

Let tmp1$ = LTrim$(RTrim$(Left$(p$, InStr(p$, "=") - 1)))

If op$ = "" Then

    Let tmp2$ = LTrim$(RTrim$(Mid$(p$, InStr(p$, "=") + 1, Len(p$))))
    Let tmp3$ = ""

Else

    Let tmp2$ = LTrim$(RTrim$(Mid$(p$, InStr(p$, "=") + 1, InStr(p$, op$) - InStr(p$, "=") - 1)))
    Let tmp3$ = LTrim$(RTrim$(Mid$(p$, InStr(p$, op$) + 1, Len(p$))))

End If

Let pointvar1 = -1
Let pointvar2 = -1
Let pointvar3 = -1

For check = 0 To vars - 1

    If UCase$(tmp1$) = UCase$(varname$(check)) Then Let pointvar1 = check
    If UCase$(tmp2$) = UCase$(varname$(check)) Then Let pointvar2 = check
    If UCase$(tmp3$) = UCase$(varname$(check)) Then Let pointvar3 = check

Next check

If pointvar1 <> -1 And pointvar3 = -1 And op$ = "" Then

    If pointvar2 = -1 Then

        Let varval(pointvar1) = Val(tmp2$)

    Else

        Let varval(pointvar1) = varval(pointvar2)

    End If

ElseIf pointvar1 <> -1 And op$ = "+" Then

    If pointvar2 <> -1 And pointvar3 <> -1 Then

        Let varval(pointvar1) = varval(pointvar2) + varval(pointvar3)

    ElseIf pointvar2 = -1 And pointvar3 <> -1 Then

        Let varval(pointvar1) = Val(tmp2$) + varval(pointvar3)

    ElseIf pointvar2 <> -1 And pointvar3 = -1 Then

        Let varval(pointvar1) = varval(pointvar2) + Val(tmp3$)

    ElseIf pointvar2 = -1 And pointvar3 = -1 Then

        Let varval(pointvar1) = Val(tmp2$) + Val(tmp3$)

    End If

ElseIf pointvar1 <> -1 And op$ = "-" Then

    If pointvar2 <> -1 And pointvar3 <> -1 Then

        Let varval(pointvar1) = varval(pointvar2) - varval(pointvar3)

    ElseIf pointvar2 = -1 And pointvar3 <> -1 Then

        Let varval(pointvar1) = Val(tmp2$) - varval(pointvar3)

    ElseIf pointvar2 <> -1 And pointvar3 = -1 Then

        Let varval(pointvar2) = varval(pointvar2) - Val(tmp3$)

    ElseIf pointvar2 = -1 And pointvar3 = -1 Then

        Let varval(pointvar2) = Val(tmp2$) - Val(tmp3$)

    End If

ElseIf pointvar1 <> -1 And op$ = "*" Then

    If pointvar2 <> -1 And pointvar3 <> -1 Then

        Let varval(pointvar1) = varval(pointvar2) * varval(pointvar3)

    ElseIf pointvar2 = -1 And pointvar3 <> -1 Then

        Let varval(pointvar1) = Val(tmp2$) * varval(pointvar3)

    ElseIf pointvar2 <> -1 And pointvar3 = -1 Then

        Let varval(pointvar2) = varval(pointvar2) * Val(tmp3$)

    ElseIf pointvar2 = -1 And pointvar3 = -1 Then

        Let varval(pointvar2) = Val(tmp2$) * Val(tmp3$)

    End If

ElseIf pointvar1 <> -1 And op$ = "/" Then

    If pointvar2 <> -1 And pointvar3 <> -1 Then

        Let varval(pointvar1) = varval(pointvar2) / varval(pointvar3)

    ElseIf pointvar2 = -1 And pointvar3 <> -1 Then

        Let varval(pointvar1) = Val(tmp2$) / varval(pointvar3)

    ElseIf pointvar2 <> -1 And pointvar3 = -1 Then

        Let varval(pointvar2) = varval(pointvar2) / Val(tmp3$)

    ElseIf pointvar2 = -1 And pointvar3 = -1 Then

        Let varval(pointvar2) = Val(tmp2$) / Val(tmp3$)

    End If

End If

Return

executeif:

Let op$ = ""

If InStr(p$, "=") Then Let op$ = "="
If InStr(p$, "<") Then Let op$ = "<"
If InStr(p$, ">") Then Let op$ = ">"
If InStr(p$, "<>") Then Let op$ = "<>"
If InStr(p$, "<=") Then Let op$ = "<="
If InStr(p$, ">=") Then Let op$ = ">="

Let ol = Len(op$)

Let tmp1$ = LTrim$(RTrim$(Left$(p$, InStr(p$, op$) - 1)))
Let tmp2$ = LTrim$(RTrim$(Mid$(p$, InStr(p$, op$) + ol, InStr(p$, ":") - InStr(p$, op$) - ol)))
Let tmp3$ = LTrim$(RTrim$(Mid$(p$, InStr(p$, ":") + 1, Len(p$))))

Let pointvar1 = -1
Let pointvar2 = -1

For check = 0 To vars - 1

    If UCase$(tmp1$) = UCase$(varname$(check)) Then Let pointvar1 = check
    If UCase$(tmp2$) = UCase$(varname$(check)) Then Let pointvar2 = check

Next check

Let target$ = ""

If pointvar1 <> -1 And pointvar2 = -1 Then

    If varval(pointvar1) = Val(tmp2$) And op$ = "=" Then Let target$ = tmp3$
    If varval(pointvar1) > Val(tmp2$) And op$ = ">" Then Let target$ = tmp3$
    If varval(pointvar1) < Val(tmp2$) And op$ = "<" Then Let target$ = tmp3$
    If varval(pointvar1) <> Val(tmp2$) And op$ = "<>" Then Let target$ = tmp3$
    If varval(pointvar1) >= Val(tmp2$) And op$ = ">=" Then Let target$ = tmp3$
    If varval(pointvar1) <= Val(tmp2$) And op$ = "<=" Then Let target$ = tmp3$

ElseIf pointvar1 = -1 And pointvar2 <> -1 Then

    If Val(tmp1$) = varval(pointvar2) And op$ = "=" Then Let target$ = tmp3$
    If Val(tmp1$) > varval(pointvar2) And op$ = ">" Then Let target$ = tmp3$
    If Val(tmp1$) < varval(pointvar2) And op$ = "<" Then Let target$ = tmp3$
    If Val(tmp1$) <> varval(pointvar2) And op$ = "<>" Then Let target$ = tmp3$
    If Val(tmp1$) >= varval(pointvar2) And op$ = ">=" Then Let target$ = tmp3$
    If Val(tmp1$) <= varval(pointvar2) And op$ = "<=" Then Let target$ = tmp3$

ElseIf pointvar1 = -1 And pointvar2 = -2 Then

    If Val(tmp1$) = Val(tmp2$) And op$ = "=" Then Let target$ = tmp3$
    If Val(tmp1$) > Val(tmp2$) And op$ = ">" Then Let target$ = tmp3$
    If Val(tmp1$) < Val(tmp2$) And op$ = "<" Then Let target$ = tmp3$
    If Val(tmp1$) <> Val(tmp2$) And op$ = "<>" Then Let target$ = tmp3$
    If Val(tmp1$) >= Val(tmp2$) And op$ = ">=" Then Let target$ = tmp3$
    If Val(tmp1$) <= Val(tmp2$) And op$ = "<=" Then Let target$ = tmp3$

ElseIf pointvar1 <> -1 And pointvar2 <> -2 Then

    If varval(pointvar1) = varval(pointvar2) And op$ = "=" Then Let target$ = tmp3$
    If varval(pointvar1) > varval(pointvar2) And op$ = ">" Then Let target$ = tmp3$
    If varval(pointvar1) < varval(pointvar2) And op$ = "<" Then Let target$ = tmp3$
    If varval(pointvar1) <> varval(pointvar2) And op$ = "<>" Then Let target$ = tmp3$
    If varval(pointvar1) >= varval(pointvar2) And op$ = ">=" Then Let target$ = tmp3$
    If varval(pointvar1) <= varval(pointvar2) And op$ = "<=" Then Let target$ = tmp3$

End If

If target$ <> "" Then

    For check = 0 To lines - 1

        If UCase$(cmd$(check)) = "LABEL" And UCase$(par$(check)) = UCase$(target$) Then

            Let pointline = check

            Exit For

        End If

    Next check

End If

Return

executegoto:

For check = 0 To lines - 1

    If UCase$(cmd$(check)) = "LABEL" And UCase$(par$(check)) = UCase$(p$) Then

        Let pointline = check

        Exit For

    End If

Next check

Return

executerandom:

Let comma = InStr(p$, ",")
Let name$ = Left$(p$, comma - 1)
Let p$ = Right$(p$, Len(p$) - comma)

Let comma = InStr(p$, ",")
Let range1 = Val(Left$(p$, comma - 1))
Let p$ = Right$(p$, Len(p$) - comma)

Let range2 = Val(p$)

For check = 0 To vars - 1

    If UCase$(varname$(check)) = UCase$(name$) Then

        Let varval(check) = Int(Rnd * range2) + range1

        Exit For

    End If

Next check

Return

executeoutput:

If Left$(p$, 1) = "$" Then

    For check = 0 To vars - 1

        If UCase$(strname$(check)) = UCase$(Right$(p$, Len(p$) - 1)) Then

            Let page$ = page$ + strval$(check)

        End If

    Next check

ElseIf Left$(p$, 1) = "*" Then

    For check = 0 To vars - 1

        If UCase$(varname$(check)) = UCase$(Right$(p$, Len(p$) - 1)) Then

            Let page$ = page$ + Str$(varval(check))

        End If

    Next check

Else

    Let page$ = page$ + p$

End If

Return

executeformget:

If InStr(UCase$(urlpar$), UCase$(p$)) Then

    Let decode$ = ""

    For check = 0 To vars - 1

        If UCase$(strname$(check)) = UCase$(p$) Then

            For check2 = InStr(UCase$(urlpar$), UCase$(p$)) + Len(p$) + 1 To Len(urlpar$)

                If Mid$(urlpar$, check2, 1) <> "&" Then

                    Let decode$ = decode$ + Mid$(urlpar$, check2, 1)

                Else

                    Exit For

                End If

            Next check2

            Let strval$(check) = decode$

            Exit For

        End If

    Next check

End If

Return

executeformpost:

Return

executedate:

For check = 0 To vars - 1

    If UCase$(strname$(check)) = UCase$(p$) Then

        Let strval$(check) = Date$

        Exit For

    End If

Next check

Return

executetime:

For check = 0 To vars - 1

    If UCase$(strname$(check)) = UCase$(p$) Then

        Let strval$(check) = Time$

        Exit For

    End If

Next check

Return

executenewfile:

If Left$(p$, 1) = "$" Then

    For check = 0 To vars - 1

        If UCase$(strname$(check)) = UCase$(p$) Then

            Open strval$(check) For Output As #2

            Exit For

        End If

    Next check

Else

    Open "www/" + p$ For Output As #2

End If

Return

executeloadfile:

If Left$(p$, 1) = "$" Then

    For check = 0 To vars - 1

        If UCase$(strname$(check)) = UCase$(p$) Then

            Open strval$(check) For Input As #2

            Exit For

        End If

    Next check

Else

    Open "www/" + p$ For Input As #2

End If

Return

executeappendfile:

If Left$(p$, 1) = "$" Then

    For check = 0 To vars - 1

        If UCase$(strname$(check)) = UCase$(p$) Then

            Open strval$(check) For Append As #2

            Exit For

        End If

    Next check

Else

    Open "www/" + p$ For Append As #2

End If

Return

executeclosefile:

Close #2

Return

executegetstring:

For check = 0 To vars - 1

    If UCase$(strname$(check)) = UCase$(p$) Then

        Line Input #2, strval$(check)

        Exit For

    End If

Next check

Return

executeputstring:

If Left$(p$, 1) = "$" Then

    For check = 0 To vars - 1

        If UCase$(strname$(check)) = UCase$(Right$(p$, Len(p$) - 1)) Then

            Print #2, strval$(check)

        End If

    Next check

ElseIf Left$(p$, 1) = "*" Then

    For check = 0 To vars - 1

        If UCase$(varname$(check)) = UCase$(Right$(p$, Len(p$) - 1)) Then

            Print #2, Str$(varval(check))

        End If

    Next check

Else

    Print #2, p$

End If

Return

executecheckfile:

For check = 0 To vars - 1

    If UCase$(varname$(check)) = UCase$(p$) Then

        Let varval(check) = EOF(2)

        Exit For

    End If

Next check

Return

checksyntax:

If c$ <> "" And UCase$(c$) <> "REM" And UCase$(c$) <> "END" And UCase$(c$) <> "LABEL" Then

    Let page$ = "<!DOCTYPE html><html><body><b>Error</b><br />Bad command on line #" + Str$(pointline) + "</body></html>"

End If

Return

decodeurl:

If InStr(indata$, "?") Then

    Let urlpar$ = Right$(indata$, Len(indata$) - InStr(indata$, "?"))
    Let indata$ = Left$(indata$, InStr(indata$, "?") - 1)

    Let decode$ = ""

    For check = 1 To Len(urlpar$)

        If Len(urlpar$) - check >= 2 Then

            Select Case UCase$(Mid$(urlpar$, check, 3))

                Case "%20": Let decode$ = decode$ + " ": Let check = check + 2
                Case "%21": Let decode$ = decode$ + "!": Let check = check + 2
                Case "%22": Let decode$ = decode$ + Chr$(34): Let check = check + 2
                Case "%23": Let decode$ = decode$ + "#": Let check = check + 2
                Case "%24": Let decode$ = decode$ + "$": Let check = check + 2
                Case "%25": Let decode$ = decode$ + "%": Let check = check + 2
                Case "%26": Let decode$ = decode$ + "&": Let check = check + 2
                Case "%27": Let decode$ = decode$ + "'": Let check = check + 2
                Case "%28": Let decode$ = decode$ + "(": Let check = check + 2
                Case "%29": Let decode$ = decode$ + ")": Let check = check + 2
                Case "%2A": Let decode$ = decode$ + "*": Let check = check + 2
                Case "%2B": Let decode$ = decode$ + "+": Let check = check + 2
                Case "%2C": Let decode$ = decode$ + ",": Let check = check + 2
                Case "%2D": Let decode$ = decode$ + "-": Let check = check + 2
                Case "%2E": Let decode$ = decode$ + ".": Let check = check + 2
                Case "%2F": Let decode$ = decode$ + "/": Let check = check + 2
                Case "%3A": Let decode$ = decode$ + ":": Let check = check + 2
                Case "%3B": Let decode$ = decode$ + ";": Let check = check + 2
                Case "%3C": Let decode$ = decode$ + "<": Let check = check + 2
                Case "%3D": Let decode$ = decode$ + "=": Let check = check + 2
                Case "%3E": Let decode$ = decode$ + ">": Let check = check + 2
                Case "%3F": Let decode$ = decode$ + "?": Let check = check + 2
                Case "%40": Let decode$ = decode$ + "@": Let check = check + 2
                Case Else

                    If Mid$(urlpar$, check, 1) = "+" Then

                        Let decode$ = decode$ + " "

                    Else

                        Let decode$ = decode$ + Mid$(urlpar$, check, 1)

                    End If


            End Select

        Else

            If Mid$(urlpar$, check, 1) = "+" Then

                Let decode$ = decode$ + " "

            Else

                Let decode$ = decode$ + Mid$(urlpar$, check, 1)

            End If

        End If

    Next check

    Let urlpar$ = decode$

Else

    Let urlpar$ = ""

End If

Return

