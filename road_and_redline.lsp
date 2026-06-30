; ============================================================
; 项目红线 + 道路范围 CAD 绘制脚本
; 来源：项目范围坐标图 + 红线与道路关系图
; 坐标对应：CAD X = 表格横坐标，CAD Y = 表格纵坐标
; 道路宽度：按图目测估算 25m（可调整）
; ============================================================

; ---------- 红线绘制函数 ----------
(defun C:DRAW_REDLINE ( / mspace pt_list pts_array idx redline i pt_count pt_name textobj area_text)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
  
  ; 定义红线坐标点列表 (CAD_X=表格横坐标, CAD_Y=表格纵坐标)
  (setq pt_list '(
    (-10898.580  8123.279)   ; J1
    (-10687.453  8115.266)   ; J2
    (-10730.994  7987.060)   ; J3
    (-10761.708  7966.796)   ; J4
    (-10819.231  7971.233)   ; J5
    (-10821.264  7971.383)   ; J6
    (-10821.180  7973.437)   ; J7
    (-10818.231  8062.941)   ; J8
    (-10898.883  8066.043)   ; J9
    (-10900.728  8066.104)   ; J10
    (-10898.580  8123.279)   ; 回到J1
  ))
  
  ; 创建闭合红线
  (setq pts_array (vlax-make-safearray vlax-vbdouble (cons 0 (1- (* 2 (length pt_list)))))
  (setq idx 0)
  (foreach pt pt_list
    (vlax-safearray-put-element pts_array idx (car pt))
    (vlax-safearray-put-element pts_array (1+ idx) (cadr pt))
    (setq idx (+ idx 2))
  )
  
  (setq redline (vla-addlightweightpolyline mspace pts_array))
  (vla-put-closed redline :vlax-true)
  (vla-put-color redline acRed)
  
  ; 标注点号
  (setq i 1)
  (setq pt_count (length pt_list))
  (foreach pt (reverse (cdr (reverse pt_list)))
    (if (< i pt_count)
      (progn
        (setq pt_name (strcat "J" (itoa i)))
        (setq textobj (vla-addtext mspace pt_name
          (vlax-3d-point (list (car pt) (+ (cadr pt) 3.0) 0.0))
          3.0
        ))
        (vla-put-color textobj acYellow)
        (setq i (1+ i))
      )
    )
  )
  
  ; 计算面积
  (command "_.AREA" "_O" (vlax-vla-object-to-ename redline))
  (setq area (getvar "AREA"))
  
  ; 标注面积
  (setq area_text (vla-addtext mspace
    (strcat "红线面积: " (rtos (abs area) 2 2) " m²")
    (vlax-3d-point (list -10920.0 8020.0 0.0))
    4.0
  ))
  (vla-put-color area_text acGreen)
  
  (princ (strcat "\n=== 红线绘制完成 ==="))
  (princ (strcat "\n面积: " (rtos (abs area) 2 2) " 平方米"))
  (princ (strcat "\n约 " (rtos (/ (abs area) 666.7) 2 2) " 亩"))
  (princ "\n")
  (princ)
)

; ---------- 道路绘制函数 ----------
(defun C:DRAW_ROAD ( / mspace road_pts road_pts_array idx road_poly road_area textobj)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
  
  ; 道路坐标点列表（内边界 + 外边界，闭合多段线）
  ; 道路宽度：按图目测估算 25m（可调整下方偏移值）
  (setq road_pts '(
    ; --- 内边界（与红线共享）---
    (-10687.453  8115.266)    ; J2 - 内边界起点
    (-10898.580  8123.279)    ; J1 - 内边界
    (-10900.728  8066.104)    ; J10 - 内边界
    (-10898.883  8066.043)    ; J9 - 内边界
    (-10818.231  8062.941)    ; J8 - 内边界终点
    ; --- 外边界（偏移 25m）---
    (-10818.231  8037.941)    ; J8 外 - 向南偏移 25m
    (-10898.883  8041.043)    ; J9 外 - 向南偏移 25m
    (-10900.728  8041.104)    ; J10 外 - 向南偏移 25m
    (-10898.580  8098.279)    ; J1 外南 - 向南偏移 25m
    (-10873.580  8123.279)    ; J1 外东 - 向东偏移 25m
    (-10662.453  8115.266)    ; J2 外 - 向东偏移 25m
  ))
  
  ; 创建道路多段线
  (setq road_pts_array (vlax-make-safearray vlax-vbdouble (cons 0 (1- (* 2 (length road_pts)))))
  (setq idx 0)
  (foreach pt road_pts
    (vlax-safearray-put-element road_pts_array idx (car pt))
    (vlax-safearray-put-element road_pts_array (1+ idx) (cadr pt))
    (setq idx (+ idx 2))
  )
  
  (setq road_poly (vla-addlightweightpolyline mspace road_pts_array))
  (vla-put-closed road_poly :vlax-true)
  (vla-put-color road_poly acCyan)  ; 青色表示道路
  
  ; 标注道路名称
  (setq textobj (vla-addtext mspace "连路"
    (vlax-3d-point (list -10780.0 8080.0 0.0))
    5.0
  ))
  (vla-put-color textobj acCyan)
  
  ; 计算道路面积
  (command "_.AREA" "_O" (vlax-vla-object-to-ename road_poly))
  (setq road_area (getvar "AREA"))
  
  ; 标注道路面积
  (vla-addtext mspace
    (strcat "道路面积(估): " (rtos (abs road_area) 2 2) " m²")
    (vlax-3d-point (list -10780.0 8072.0 0.0))
    3.0
  )
  
  ; 标注道路宽度
  (vla-addtext mspace "道路宽度: 25m(目测估算)"
    (vlax-3d-point (list -10780.0 8064.0 0.0))
    2.5
  )
  
  (princ (strcat "\n=== 道路绘制完成 ==="))
  (princ (strcat "\n道路面积(估): " (rtos (abs road_area) 2 2) " 平方米"))
  (princ (strcat "\n约 " (rtos (/ (abs road_area) 666.7) 2 2) " 亩"))
  (princ "\n")
  (princ)
)

; ---------- 红线+道路一起绘制 ----------
(defun C:DRAW_ALL ()
  (C:DRAW_REDLINE)
  (C:DRAW_ROAD)
  (command "_.ZOOM" "_E")
  (princ "\n=== 红线与道路绘制全部完成 ===")
  (princ "\n")
  (princ)
)

; 加载后执行：
; 输入 DRAW_REDLINE 只绘制红线
; 输入 DRAW_ROAD 只绘制道路
; 输入 DRAW_ALL 同时绘制红线和道路
(princ "\n脚本加载成功。")
(princ "\n命令：DRAW_REDLINE = 红线 | DRAW_ROAD = 道路 | DRAW_ALL = 全部")
(princ "\n")
(princ)
