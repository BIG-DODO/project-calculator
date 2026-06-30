; ============================================================
; 项目红线自动绘制脚本（修正版：CAD坐标与表格对调）
; 来源：项目范围坐标图
; 坐标对应：CAD X = 表格横坐标，CAD Y = 表格纵坐标
; 点数量：10个（闭合）
; ============================================================
(defun C:DRAW_REDLINE ( / pt_list i pt_name p1 p2 dist total_dist mspace area_line redline textobj table_x table_y data_lines line_y)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
  
  ; 定义坐标点列表 (CAD_X=表格横坐标, CAD_Y=表格纵坐标)
  (setq pt_list '(
    (-10898.580  8123.279)   ; 1号点  横:-10898.580 纵:8123.279
    (-10687.453  8115.266)   ; 2号点  横:-10687.453 纵:8115.266
    (-10730.994  7987.060)   ; 3号点  横:-10730.994 纵:7987.060
    (-10761.708  7966.796)   ; 4号点  横:-10761.708 纵:7966.796
    (-10819.231  7971.233)   ; 5号点  横:-10819.231 纵:7971.233
    (-10821.264  7971.383)   ; 6号点  横:-10821.264 纵:7971.383
    (-10821.180  7973.437)   ; 7号点  横:-10821.180 纵:7973.437
    (-10818.231  8062.941)   ; 8号点  横:-10818.231 纵:8062.941
    (-10898.883  8066.043)   ; 9号点  横:-10898.883 纵:8066.043
    (-10900.728  8066.104)   ; 10号点 横:-10900.728 纵:8066.104
    (-10898.580  8123.279)   ; 回到1号点（闭合）
  ))
  
  ; 创建多段线（闭合红线）
  (setq pts_array (vlax-make-safearray vlax-vbdouble (cons 0 (1- (* 2 (length pt_list))))))
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
  (foreach pt pt_list
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
  
  ; 绘制坐标表格
  (setq table_x -11050.0)
  (setq table_y 8160.0)
  (setq row_height 4.5)
  
  ; 表头背景框
  (vla-addline mspace (vlax-3d-point (list table_x (+ table_y 2.0) 0.0)) (vlax-3d-point (list (+ table_x 140.0) (+ table_y 2.0) 0.0)))
  (vla-put-color (vlax-ename->vla-object (entlast)) acWhite)
  
  ; 表头文字
  (vla-addtext mspace "点号"
    (vlax-3d-point (list (+ table_x 2.0) table_y 0.0)) 2.5)
  (vla-put-color (vlax-ename->vla-object (entlast)) acWhite)
  
  (vla-addtext mspace "距离(M)"
    (vlax-3d-point (list (+ table_x 22.0) table_y 0.0)) 2.5)
  (vla-put-color (vlax-ename->vla-object (entlast)) acWhite)
  
  (vla-addtext mspace "横坐标(Y)"
    (vlax-3d-point (list (+ table_x 55.0) table_y 0.0)) 2.5)
  (vla-put-color (vlax-ename->vla-object (entlast)) acWhite)
  
  (vla-addtext mspace "纵坐标(X)"
    (vlax-3d-point (list (+ table_x 95.0) table_y 0.0)) 2.5)
  (vla-put-color (vlax-ename->vla-object (entlast)) acWhite)
  
  ; 数据行
  (setq data_lines '(
    "J1   211.28  -10898.580  8123.279"
    "J2   135.40  -10687.453  8115.266"
    "J3    36.80  -10730.994  7987.060"
    "J4    57.69  -10761.708  7966.796"
    "J5     2.04  -10819.231  7971.233"
    "J6     2.06  -10821.264  7971.383"
    "J7    89.55  -10821.180  7973.437"
    "J8    80.71  -10818.231  8062.941"
    "J9     1.85  -10898.883  8066.043"
    "J10   57.22  -10900.728  8066.104"
  ))
  
  (setq line_y (- table_y 5.0))
  (foreach line data_lines
    (vla-addtext mspace line
      (vlax-3d-point (list (+ table_x 2.0) line_y 0.0)) 2.0)
    (vla-put-color (vlax-ename->vla-object (entlast)) acWhite)
    (setq line_y (- line_y row_height))
  )
  
  ; 缩放视图到红线范围
  (command "_.ZOOM" "_E")
  
  (princ (strcat "\n=== 红线绘制完成 ==="))
  (princ (strcat "\n总点数: 10"))
  (princ (strcat "\n面积: " (rtos (abs area) 2 2) " 平方米"))
  (princ (strcat "\n约 " (rtos (/ (abs area) 666.7) 2 2) " 亩"))
  (princ (strcat "\n文件: redline_draw.lsp"))
  (princ "\n")
  (princ)
)

; 加载后执行：在命令行输入 DRAW_REDLINE
(princ "\n红线绘制脚本加载成功。输入 DRAW_REDLINE 执行绘制。")
(princ)
