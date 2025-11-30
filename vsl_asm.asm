.MODEL SMALL
.STACK 100h

.DATA
    ; --- MENU STRINGS ---
    msg_menu    DB 13, 10, '--- ASSEMBLY CHARTS ---', 13, 10
                DB '1. Bar Chart', 13, 10
                DB '2. Scatter Plot', 13, 10
                DB '3. Line Chart', 13, 10
                DB 'Select (1-3): $'
                
    ; --- INPUT PROMPTS ---
    msg_count   DB 13, 10, 'How many points? (2-5): $'
    msg_lbl     DB 13, 10, 'Label (3 chars): $'
    msg_val_x   DB 13, 10, 'X Value: $'
    msg_val_y   DB 13, 10, 'Y Value: $'
    msg_zero    DB '0$'

    ; --- CONFIG VARIABLES ---
    chart_mode  DB ?        ; 1=Bar, 2=Scatter, 3=Line
    num_points  DB ?
    max_x       DW ?
    max_y       DW ?

    ; --- BRESENHAM VARIABLES ---
    curr_x      DW ?
    curr_y      DW ?
    prev_x      DW ?
    prev_y      DW ?
    line_x0     DW ?
    line_y0     DW ?
    line_x1     DW ?
    line_y1     DW ?
    delta_x     DW ?
    delta_y     DW ?
    step_x      DW ?
    step_y      DW ?
    err_term    DW ?

    ; --- DATA ARRAYS ---
    ; Labels (Bar Chart only): 5 items * 4 bytes
    labels      DB 20 DUP (' ') 
    ; X Values (Scatter/Line): 5 items * 2 bytes
    values_x    DW 5 DUP (0)
    ; Y Values (All Charts):   5 items * 2 bytes
    values_y    DW 5 DUP (0)

.CODE
MAIN PROC
    mov ax, @data
    mov ds, ax

    ; -----------------------------------------
    ; 1. MAIN MENU
    ; -----------------------------------------
SHOW_MENU:
    mov dx, OFFSET msg_menu
    mov ah, 09h
    int 21h

    call READ_CHAR
    sub al, '0'
    mov chart_mode, al

    ; -----------------------------------------
    ; 2. GET COUNT
    ; -----------------------------------------
    mov dx, OFFSET msg_count
    mov ah, 09h
    int 21h
    
    call READ_CHAR
    sub al, '0'
    mov num_points, al

    ; -----------------------------------------
    ; 3. INPUT DATA (BRANCH)
    ; -----------------------------------------
    cmp chart_mode, 1
    je CALL_INPUT_BAR
    jmp CALL_INPUT_XY

CALL_INPUT_BAR:
    call INPUT_ROUTINE_BAR
    jmp CALC_MAX_PHASE

CALL_INPUT_XY:
    call INPUT_ROUTINE_XY
    jmp CALC_MAX_PHASE

    ; -----------------------------------------
    ; 4. CALCULATE MAX VALUES
    ; -----------------------------------------
CALC_MAX_PHASE:
    ; Find Max Y (Used by all)
    mov bx, OFFSET values_y
    call FIND_MAX_IN_ARRAY
    mov max_y, ax

    ; Find Max X (Only used by Scatter/Line)
    cmp chart_mode, 1
    je START_GRAPHICS
    
    mov bx, OFFSET values_x
    call FIND_MAX_IN_ARRAY
    mov max_x, ax

    ; -----------------------------------------
    ; 5. GRAPHICS MODE
    ; -----------------------------------------
START_GRAPHICS:
    mov ax, 0013h
    int 10h
    mov ax, 0A000h
    mov es, ax

    call DRAW_AXES_FULL

    ; -----------------------------------------
    ; 6. RENDER DISPATCH
    ; -----------------------------------------
    cmp chart_mode, 1
    je DO_BAR
    cmp chart_mode, 2
    je DO_SCATTER
    jmp DO_LINE

DO_BAR:
    call RENDER_BAR_CHART
    jmp EXIT_PGM
DO_SCATTER:
    call RENDER_SCATTER_PLOT
    jmp EXIT_PGM
DO_LINE:
    call RENDER_LINE_CHART
    jmp EXIT_PGM

EXIT_PGM:
    xor ah, ah
    int 16h             ; Wait for key
    mov ax, 0003h       ; Text mode
    int 10h
    mov ax, 4C00h       ; Exit
    int 21h
MAIN ENDP

; ==========================================================
;   INPUT ROUTINES
; ==========================================================

INPUT_ROUTINE_BAR PROC
    xor cx, cx
BAR_IN_LOOP:
    mov al, num_points
    xor ah, ah
    cmp cx, ax
    jge BAR_IN_DONE
    
    push cx
    
    ; Get Label
    mov dx, OFFSET msg_lbl
    mov ah, 09h
    int 21h
    
    ; Calc Address: labels + (cx*4)
    mov ax, cx
    mov bl, 4
    mul bl
    mov bx, OFFSET labels
    add bx, ax
    
    call READ_CHAR
    mov [bx], al
    call READ_CHAR
    mov [bx+1], al
    call READ_CHAR
    mov [bx+2], al
    mov byte ptr [bx+3], '$'
    
    ; Get Y Value
    mov dx, OFFSET msg_val_y
    mov ah, 09h
    int 21h
    call READ_INT
    
    mov bx, OFFSET values_y
    pop cx
    push cx
    add bx, cx
    add bx, cx
    mov [bx], ax
    
    pop cx
    inc cx
    jmp BAR_IN_LOOP
BAR_IN_DONE:
    ret
INPUT_ROUTINE_BAR ENDP

INPUT_ROUTINE_XY PROC
    xor cx, cx
XY_IN_LOOP:
    mov al, num_points
    xor ah, ah
    cmp cx, ax
    jge XY_IN_DONE
    
    push cx
    ; Get X
    mov dx, OFFSET msg_val_x
    mov ah, 09h
    int 21h
    call READ_INT
    mov bx, OFFSET values_x
    pop cx
    push cx
    add bx, cx
    add bx, cx
    mov [bx], ax
    
    ; Get Y
    mov dx, OFFSET msg_val_y
    mov ah, 09h
    int 21h
    call READ_INT
    mov bx, OFFSET values_y
    pop cx
    push cx
    add bx, cx
    add bx, cx
    mov [bx], ax
    
    pop cx
    inc cx
    jmp XY_IN_LOOP
XY_IN_DONE:
    ret
INPUT_ROUTINE_XY ENDP

; ==========================================================
;   RENDERING ROUTINES
; ==========================================================

RENDER_BAR_CHART PROC
    xor cx, cx
BAR_DRAW_LOOP:
    mov al, num_points
    xor ah, ah
    cmp cx, ax
    jge BAR_DRAW_DONE
    
    push cx
    
    ; Calc Height: (ValueY * 180) / MaxY
    mov bx, OFFSET values_y
    add bx, cx
    add bx, cx
    mov ax, [bx]
    mov dx, 180
    mul dx
    div max_y
    mov dx, ax      ; DX = Height
    
    ; Calc X: (Index * 50) + 30
    mov ax, cx
    mov bl, 50
    mul bl
    add ax, 30
    mov si, ax      ; SI = Start X
    
    ; Draw Bar
    mov al, dl      ; Height
    mov bl, 32      ; Color
    add bl, cl
    call DRAW_RECT_FUNC
    
    ; Draw Label
    ; Col = PixelX / 8
    mov ax, si
    mov bl, 8
    div bl
    mov dl, al
    mov dh, 23
    
    mov ax, cx
    mov bl, 4
    mul bl
    mov bx, OFFSET labels
    add bx, ax
    
    call SET_CURSOR
    mov al, [bx]
    call PRINT_CHAR_GFX
    mov al, [bx+1]
    call PRINT_CHAR_GFX
    mov al, [bx+2]
    call PRINT_CHAR_GFX
    
    pop cx
    inc cx
    jmp BAR_DRAW_LOOP
BAR_DRAW_DONE:
    ret
RENDER_BAR_CHART ENDP

RENDER_SCATTER_PLOT PROC
    xor cx, cx
SCAT_DRAW_LOOP:
    mov al, num_points
    xor ah, ah
    cmp cx, ax
    jge SCAT_DRAW_DONE
    push cx
    
    call CALCULATE_XY_PIXELS ; Returns SI=X, DX=Y
    
    ; Draw Cross
    mov bl, 40 ; Red
    add bl, cl
    call DRAW_CROSS_FUNC
    
    pop cx
    inc cx
    jmp SCAT_DRAW_LOOP
SCAT_DRAW_DONE:
    ret
RENDER_SCATTER_PLOT ENDP

RENDER_LINE_CHART PROC
    xor cx, cx
LINE_DRAW_LOOP:
    mov al, num_points
    xor ah, ah
    cmp cx, ax
    jge LINE_DRAW_DONE
    push cx
    
    call CALCULATE_XY_PIXELS
    
    ; Draw Dot
    mov curr_x, si
    mov curr_y, dx
    mov bl, 15
    call PLOT_PIXEL_SAFE
    
    pop cx
    cmp cx, 0
    je FIRST_LINE_PT
    
    ; Draw Line from Prev to Curr
    mov ax, prev_x
    mov line_x0, ax
    mov ax, prev_y
    mov line_y0, ax
    mov ax, curr_x
    mov line_x1, ax
    mov ax, curr_y
    mov line_y1, ax
    
    mov bl, 44 ; Yellow
    call DRAW_BRESENHAM
    
FIRST_LINE_PT:
    mov ax, curr_x
    mov prev_x, ax
    mov ax, curr_y
    mov prev_y, ax
    
    inc cx
    jmp LINE_DRAW_LOOP
LINE_DRAW_DONE:
    ret
RENDER_LINE_CHART ENDP

; ==========================================================
;   HELPER FUNCTIONS
; ==========================================================

CALCULATE_XY_PIXELS PROC
    ; Reads Index form Stack (CX is valid)
    ; Returns SI = Pixel X, DX = Pixel Y
    
    ; X Calc
    mov bx, OFFSET values_x
    add bx, cx
    add bx, cx
    mov ax, [bx]
    mov dx, 280
    mul dx
    mov di, max_x
    cmp di, 0
    jne DIV_X_S
    mov di, 1
DIV_X_S:
    div di
    add ax, 20
    mov si, ax
    
    ; Y Calc
    mov bx, OFFSET values_y
    add bx, cx
    add bx, cx
    mov ax, [bx]
    mov dx, 180
    mul dx
    mov di, max_y
    cmp di, 0
    jne DIV_Y_S
    mov di, 1
DIV_Y_S:
    div di
    mov dx, 190
    sub dx, ax
    ret
CALCULATE_XY_PIXELS ENDP

DRAW_CROSS_FUNC PROC
    push ax
    push di
    push dx
    push si
    call PLOT_PIXEL_SAFE
    dec si
    call PLOT_PIXEL_SAFE
    add si, 2
    call PLOT_PIXEL_SAFE
    dec si
    dec dx
    call PLOT_PIXEL_SAFE
    add dx, 2
    call PLOT_PIXEL_SAFE
    pop si
    pop dx
    pop di
    pop ax
    ret
DRAW_CROSS_FUNC ENDP

DRAW_RECT_FUNC PROC
    ; AL=Height, SI=X, BL=Color
    push ax
    push bx
    push cx
    push dx
    push di
    
    cmp al, 0
    je RECT_DONE
    xor ah, ah
    mov dx, 190
    sub dx, ax
RECT_Y_LOOP:
    cmp dx, 190
    jge RECT_DONE
    mov cx, si
    mov di, 0
RECT_X_LOOP:
    cmp di, 20
    je RECT_NEXT
    push dx
    push ax
    mov ax, 320
    mul dx
    add ax, cx
    push di
    mov di, ax
    mov byte ptr es:[di], bl
    pop di
    pop ax
    pop dx
    inc cx
    inc di
    jmp RECT_X_LOOP
RECT_NEXT:
    inc dx
    jmp RECT_Y_LOOP
RECT_DONE:
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
DRAW_RECT_FUNC ENDP

DRAW_BRESENHAM PROC
    ; Delta X
    mov ax, line_x1
    sub ax, line_x0
    jns B_POS_DX
    neg ax
B_POS_DX:
    mov delta_x, ax
    
    ; Step X
    mov ax, line_x0
    cmp ax, line_x1
    jl B_STEP_X_POS
    mov step_x, -1
    jmp B_CALC_DY
B_STEP_X_POS:
    mov step_x, 1

B_CALC_DY:
    mov ax, line_y1
    sub ax, line_y0
    jns B_POS_DY
    neg ax
B_POS_DY:
    neg ax
    mov delta_y, ax
    
    mov ax, line_y0
    cmp ax, line_y1
    jl B_STEP_Y_POS
    mov step_y, -1
    jmp B_INIT
B_STEP_Y_POS:
    mov step_y, 1

B_INIT:
    mov ax, delta_x
    add ax, delta_y
    mov err_term, ax

B_LOOP:
    mov si, line_x0
    mov dx, line_y0
    call PLOT_PIXEL_SAFE
    
    mov ax, line_x0
    cmp ax, line_x1
    jne B_NEXT
    mov ax, line_y0
    cmp ax, line_y1
    je B_DONE

B_NEXT:
    mov di, err_term
    add di, di
    
    mov ax, delta_y
    cmp di, ax
    jl B_CHECK_Y
    mov ax, delta_y
    add err_term, ax
    mov ax, line_x0
    add ax, step_x
    mov line_x0, ax

B_CHECK_Y:
    mov ax, delta_x
    cmp di, ax
    jg B_LOOP
    mov ax, delta_x
    add err_term, ax
    mov ax, line_y0
    add ax, step_y
    mov line_y0, ax
    jmp B_LOOP
B_DONE:
    ret
DRAW_BRESENHAM ENDP

PLOT_PIXEL_SAFE PROC
    push ax
    push dx
    push di
    cmp si, 319
    jg P_SKIP
    cmp dx, 199
    jg P_SKIP
    mov ax, 320
    mul dx
    add ax, si
    mov di, ax
    mov byte ptr es:[di], bl
P_SKIP:
    pop di
    pop dx
    pop ax
    ret
PLOT_PIXEL_SAFE ENDP

DRAW_AXES_FULL PROC
    ; Lines
    push ax
    push dx
    push di
    mov dx, 10
Y_AXIS:
    cmp dx, 190
    je X_AXIS
    push dx
    mov ax, 320
    mul dx
    add ax, 20
    mov di, ax
    mov byte ptr es:[di], 15
    pop dx
    inc dx
    jmp Y_AXIS
X_AXIS:
    mov cx, 20
X_LOOP:
    cmp cx, 310
    je AXES_LBL
    mov di, 60800
    add di, cx
    mov byte ptr es:[di], 15
    inc cx
    jmp X_LOOP
AXES_LBL:
    pop di
    pop dx
    pop ax
    
    ; Labels
    mov dh, 23
    mov dl, 1
    call SET_CURSOR
    mov dx, OFFSET msg_zero
    mov ah, 09h
    int 21h
    
    mov dh, 1
    mov dl, 1
    call SET_CURSOR
    mov ax, max_y
    call PRINT_NUM
    
    ; Only print Max X if not Bar chart
    cmp chart_mode, 1
    je AXES_DONE
    mov dh, 23
    mov dl, 35
    call SET_CURSOR
    mov ax, max_x
    call PRINT_NUM
AXES_DONE:
    ret
DRAW_AXES_FULL ENDP

FIND_MAX_IN_ARRAY PROC
    push cx
    push dx
    mov cx, 0
    mov ax, 1
MAX_SCAN:
    mov dl, num_points
    xor dh, dh
    cmp cx, dx
    jge MAX_DONE
    mov dx, [bx]
    cmp dx, ax
    jle MAX_NEXT
    mov ax, dx
MAX_NEXT:
    add bx, 2
    inc cx
    jmp MAX_SCAN
MAX_DONE:
    pop dx
    pop cx
    ret
FIND_MAX_IN_ARRAY ENDP

READ_CHAR PROC
    mov ah, 01h
    int 21h
    ret
READ_CHAR ENDP

READ_INT PROC
    xor dx, dx
RI_LOOP:
    mov ah, 01h
    int 21h
    cmp al, 13
    je RI_DONE
    sub al, '0'
    xor ah, ah
    mov cx, ax
    mov ax, dx
    mov bx, 10
    mul bx
    add ax, cx
    mov dx, ax
    jmp RI_LOOP
RI_DONE:
    mov ax, dx
    ret
READ_INT ENDP

PRINT_NUM PROC
    cmp ax, 0
    jne PN_REC
    ret
PN_REC:
    mov dx, 0
    mov bx, 10
    div bx
    push dx
    cmp ax, 0
    je PN_DIG
    call PRINT_NUM
PN_DIG:
    pop dx
    add dl, '0'
    mov al, dl
    call PRINT_CHAR_GFX
    ret
PRINT_NUM ENDP

SET_CURSOR PROC
    push ax
    push bx
    mov ah, 02h
    mov bh, 0
    int 10h
    pop bx
    pop ax
    ret
SET_CURSOR ENDP

PRINT_CHAR_GFX PROC
    push ax
    push bx
    push cx
    mov ah, 0Eh
    mov bl, 15
    int 10h
    pop cx
    pop bx
    pop ax
    ret
PRINT_CHAR_GFX ENDP

END MAIN