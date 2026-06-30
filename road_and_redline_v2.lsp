; ============================================================
; 项目红线 + 道路范围 CAD 绘制脚本（精确版）
; 坐标系：CAD坐标 (X=横坐标/东向, Y=纵坐标/北向)
; 红线：10点闭合，东南角(J10)有R=30圆弧
; 东侧道路：宽18m，距红线3m
; 南侧道路：宽20m，距红线5m
; 交叉口圆弧：推测R=25m
; ============================================================

; ---------- 辅助函数：点偏移 ----------
(defun pt-offset (pt dx dy)
  (list (+ (car pt) dx) (+ (cadr pt) dy))
)

; ---------- 辅助函数：向量单位化后乘以长度 ----------
(defun vec-scale (p1 p2 dist / dx dy len)
  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))
  (setq len (sqrt (+ (* dx dx) (* dy dy))))
  (list (* (/ dx len) dist) (* (/ dy len) dist))
)

; ---------- 辅助函数：垂直偏移（顺时针=右侧） ----------
(defun perp-offset (p1 p2 dist / dx dy len nx ny)
  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))
  (setq len (sqrt (+ (* dx dx) (* dy dy))))
  (setq nx (/ dx len))
  (setq ny (/ dy len))
  ; 顺时针90° = (ny, -nx)
  (list (* dist ny) (* (- 0.0 nx) dist))
)

; ---------- 红线绘制函数 ----------
(defun C:DRAW_REDLINE_V2 ( / mspace redline arc area textobj)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
  
  ; 定义关键点（CAD坐标：X=横坐标, Y=纵坐标）
  (setq J1 (list -10898.580 8123.279))
  (setq J2 (list -10687.453 8115.266))
  (setq J3 (list -10730.994 7987.060))
  (setq J4 (list -10761.708 7966.796))
  (setq J5 (list -10819.231 7971.233))
  (setq J6 (list -10821.264 7971.383))
  (setq J7 (list -10821.180 7973.437))
  (setq J8 (list -10818.231 8062.941))
  (setq J9 (list -10898.883 8066.043))
  (setq J10 (list -10900.728 8066.104))
  
  ; J10处R=30圆弧参数（东南角）
  ; 切点1 = J10向西30m, 切点2 = J10向北30m
  (setq tangent1 (list -10930.728 8066.104))
  (setq tangent2 (list -10900.728 8096.104))
  (setq arc-center (list -10930.728 8096.104))
  
  ; 创建红线多段线（直线段 + 圆弧段）
  ; 使用PLINE命令绘制
  (command "_.PLINE"
    (car J1) (cadr J1)
    (car J2) (cadr J2)
    (car J3) (cadr J3)
    (car J4) (cadr J4)
    (car J5) (cadr J5)
    (car J6) (cadr J6)
    (car J7) (cadr J7)
    (car J8) (cadr J8)
    (car J9) (cadr J9)
    (car tangent1) (cadr tangent1)
    "_A" "_CE" (car arc-center) (cadr arc-center)
    (car tangent2) (cadr tangent2)
    (car J1) (cadr J1)
    "_C"
  )
  (setq redline (entlast))
  (command "_.PEDIT" redline "_W" 0.5 "")
  (vla-put-color (vlax-ename->vla-object redline) acRed)
  
  ; 标注点号
  (mapcar
    '(lambda (pt num)
       (setq textobj (vla-addtext mspace (strcat "J" (itoa num))
         (vlax-3d-point (list (car pt) (+ (cadr pt) 3.0) 0.0)) 3.0))
       (vla-put-color textobj acYellow)
     )
    (list J1 J2 J3 J4 J5 J6 J7 J8 J9 J10)
    '(1 2 3 4 5 6 7 8 9 10)
  )
  
  ; 标注圆弧信息
  (setq textobj (vla-addtext mspace "R=30(红线圆弧)"
    (vlax-3d-point (list -10920.0 8075.0 0.0)) 3.0))
  (vla-put-color textobj acYellow)
  
  ; 计算面积
  (command "_.AREA" "_O" redline)
  (setq area (getvar "AREA"))
  
  (setq textobj (vla-addtext mspace
    (strcat "红线面积: " (rtos (abs area) 2 2) " m²")
    (vlax-3d-point (list -10920.0 8020.0 0.0)) 4.0))
  (vla-put-color textobj acGreen)
  
  (princ (strcat "\n=== 红线绘制完成 ==="))
  (princ (strcat "\n面积: " (rtos (abs area) 2 2) " m²"))
  (princ (strcat "\n约 " (rtos (/ (abs area) 666.7) 2 2) " 亩"))
  (princ "\n")
  (princ)
)

; ---------- 道路绘制函数 ----------
(defun C:DRAW_ROAD_V2 ( / mspace)
  (setq mspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
  
  ; 定义关键点
  (setq J1 (list -10898.580 8123.279))
  (setq J2 (list -10687.453 8115.266))
  (setq J8 (list -10818.231 8062.941))
  (setq J9 (list -10898.883 8066.043))
  (setq J10 (list -10900.728 8066.104))
  
  ; 红线J10处圆弧参数
  (setq tangent1 (list -10930.728 8066.104))
  (setq tangent2 (list -10900.728 8096.104))
  (setq arc-center (list -10930.728 8096.104))
  
  ; ===== 东侧道路内边界（距红线3m） =====
  ; 沿J2->J1->tangent2->arc->tangent1->J9->J8, 向右侧偏移3m
  
  ; J2->J1段右侧偏移3m
  (setq J2_in_e (pt-offset J2 (car (perp-offset J2 J1 3.0)) (cadr (perp-offset J2 J1 3.0))))
  (setq J1_in_e (pt-offset J1 (car (perp-offset J2 J1 3.0)) (cadr (perp-offset J2 J1 3.0))))
  
  ; J1->tangent2段右侧偏移3m (方向与J10->J1相同，向北)
  (setq J1_tangent2 (vec-scale J10 J1 3.0))
  (setq tangent2_in_e (pt-offset tangent2 (car J1_tangent2) (cadr J1_tangent2)))
  ; 修正：垂直偏移
  (setq tangent2_in_e (list (- (car tangent2) 3.0) (cadr tangent2)))
  
  ; tangent2->tangent1圆弧，圆心向东偏移3m
  (setq arc-center_in_e (list (+ (car arc-center) 3.0) (cadr arc-center)))
  (setq tangent1_in_e (list (- (car tangent1) 3.0) (cadr tangent1)))
  
  ; J9->tangent1段右侧偏移3m (方向向西，右侧=北)
  (setq tangent1_in_e2 (list (- (car tangent1) 3.0) (cadr tangent1)))
  (setq J9_in_e (list (- (car J9) 3.0) (cadr J9)))
  
  ; J8->J9段右侧偏移3m (方向向西北，右侧=东北)
  (setq J8_in_e (pt-offset J8 (car (perp-offset J8 J9 3.0)) (cadr (perp-offset J8 J9 3.0))))
  
  ; ===== 南侧道路内边界（距红线5m） =====
  ; 沿J8->J9->tangent1->arc->tangent2->J1, 向下方偏移5m
  
  ; J8->J9段下方偏移5m (左侧偏移，因道路在下方)
  (setq J8_in_s (pt-offset J8 (car (perp-offset J9 J8 5.0)) (cadr (perp-offset J9 J8 5.0))))
  (setq J9_in_s (pt-offset J9 (car (perp-offset J9 J8 5.0)) (cadr (perp-offset J9 J8 5.0))))
  
  ; J9->tangent1段下方偏移5m (向西，下方=南)
  (setq tangent1_in_s (list (car tangent1) (- (cadr tangent1) 5.0)))
  
  ; tangent1->tangent2圆弧，圆心向南偏移5m
  (setq arc-center_in_s (list (car arc-center) (- (cadr arc-center) 5.0)))
  (setq tangent2_in_s (list (car tangent2) (- (cadr tangent2) 5.0)))
  
  ; tangent2->J1段下方偏移5m (向北，下方=东)
  ; J1在tangent2的北边，下方=东
  (setq J1_in_s (list (- (car J1) 5.0) (cadr J1)))
  
  ; ===== 东侧道路外边界（内边界+18m） =====
  (setq J2_out_e (pt-offset J2_in_e (car (perp-offset J2 J1 21.0)) (cadr (perp-offset J2 J1 21.0))))
  (setq J1_out_e (pt-offset J1_in_e (car (perp-offset J2 J1 21.0)) (cadr (perp-offset J2 J1 21.0))))
  (setq tangent2_out_e (list (- (car tangent2) 21.0) (cadr tangent2)))
  (setq arc-center_out_e (list (+ (car arc-center) 21.0) (cadr arc-center)))
  (setq tangent1_out_e (list (- (car tangent1) 21.0) (cadr tangent1)))
  (setq J9_out_e (list (- (car J9) 21.0) (cadr J9)))
  (setq J8_out_e (pt-offset J8_in_e (car (perp-offset J8 J9 21.0)) (cadr (perp-offset J8 J9 21.0))))
  
  ; ===== 南侧道路外边界（内边界+20m） =====
  (setq J8_out_s (pt-offset J8_in_s (car (perp-offset J9 J8 25.0)) (cadr (perp-offset J9 J8 25.0))))
  (setq J9_out_s (pt-offset J9_in_s (car (perp-offset J9 J8 25.0)) (cadr (perp-offset J9 J8 25.0))))
  (setq tangent1_out_s (list (car tangent1) (- (cadr tangent1) 25.0)))
  (setq arc-center_out_s (list (car arc-center) (- (cadr arc-center) 25.0)))
  (setq tangent2_out_s (list (car tangent2) (- (cadr tangent2) 25.0)))
  (setq J1_out_s (list (- (car J1) 25.0) (cadr J1)))
  
  ; ===== 交叉口圆弧参数（推测R=25m） =====
  ; 交叉口圆角连接东侧内边界和南侧内边界
  ; 圆角切点在两侧内边界线上，距理论交点25m
  
  ; 理论交点 = 两条内边界线交点
  ; 东侧内边界线方向：J1_in_e -> tangent2_in_e, 方向 (0.0375, 0.9993)
  ; 南侧内边界线方向：J9_in_s -> tangent1_in_s, 方向 (-0.9993, 0.0384)
  ; 交点 ≈ (-10897.918, 8060.993)
  (setq intersect_pt (list -10897.918 8060.993))
  
  ; 圆角起点（东侧内边界上）
  (setq fillet_start (list -10898.855 8036.024))
  ; 圆角终点（南侧内边界上）
  (setq fillet_end (list -10922.900 8061.967))
  ; 圆角圆心
  (setq fillet_center (list -10916.192 8043.957))
  
  ; ===== 绘制道路（东侧） =====
  (command "_.PLINE"
    (car J8_out_e) (cadr J8_out_e)
    (car J9_out_e) (cadr J9_out_e)
    (car tangent1_out_e) (cadr tangent1_out_e)
    "_A" "_CE" (car arc-center_out_e) (cadr arc-center_out_e)
    (car tangent2_out_e) (cadr tangent2_out_e)
    (car J1_out_e) (cadr J1_out_e)
    (car J2_out_e) (cadr J2_out_e)
    ""
  )
  (setq road_e (entlast))
  (vla-put-color (vlax-ename->vla-object road_e) acCyan)
  
  ; 绘制东侧道路内边界
  (command "_.PLINE"
    (car J8_in_e) (cadr J8_in_e)
    (car J9_in_e) (cadr J9_in_e)
    (car tangent1_in_e) (cadr tangent1_in_e)
    "_A" "_CE" (car arc-center_in_e) (cadr arc-center_in_e)
    (car tangent2_in_e) (cadr tangent2_in_e)
    (car J1_in_e) (cadr J1_in_e)
    (car J2_in_e) (cadr J2_in_e)
    ""
  )
  (setq road_e_in (entlast))
  (vla-put-color (vlax-ename->vla-object road_e_in) acCyan)
  
  ; ===== 绘制道路（南侧） =====
  (command "_.PLINE"
    (car J8_out_s) (cadr J8_out_s)
    (car J9_out_s) (cadr J9_out_s)
    (car tangent1_out_s) (cadr tangent1_out_s)
    "_A" "_CE" (car arc-center_out_s) (cadr arc-center_out_s)
    (car tangent2_out_s) (cadr tangent2_out_s)
    (car J1_out_s) (cadr J1_out_s)
    ""
  )
  (setq road_s (entlast))
  (vla-put-color (vlax-ename->vla-object road_s) acCyan)
  
  ; 绘制南侧道路内边界
  (command "_.PLINE"
    (car J8_in_s) (cadr J8_in_s)
    (car J9_in_s) (cadr J9_in_s)
    (car tangent1_in_s) (cadr tangent1_in_s)
    "_A" "_CE" (car arc-center_in_s) (cadr arc-center_in_s)
    (car tangent2_in_s) (cadr tangent2_in_s)
    (car J1_in_s) (cadr J1_in_s)
    ""
  )
  (setq road_s_in (entlast))
  (vla-put-color (vlax-ename->vla-object road_s_in) acCyan)
  
  ; 绘制交叉口圆角（R=25m）
  (command "_.ARC" "_C" (car fillet_center) (cadr fillet_center)
    (car fillet_start) (cadr fillet_start)
    (car fillet_end) (cadr fillet_end))
  (setq fillet_arc (entlast))
  (vla-put-color (vlax-ename->vla-object fillet_arc) acMagenta)
  
  ; 标注
  (setq textobj (vla-addtext mspace "连路(东侧18m+南侧20m)"
    (vlax-3d-point (list -10780.0 8080.0 0.0)) 5.0))
  (vla-put-color textobj acCyan)
  
  (setq textobj (vla-addtext mspace "交叉口圆角R=25m(推测)"
    (vlax-3d-point (list -10915.0 8045.0 0.0)) 3.0))
  (vla-put-color textobj acMagenta)
  
  (princ "\n=== 道路绘制完成 ===")
  (princ (strcat "\n东侧道路：宽18m，距红线3m"))
  (princ (strcat "\n南侧道路：宽20m，距红线5m"))
  (princ (strcat "\n交叉口圆角：R=25m（推测）"))
  (princ "\n")
  (princ)
)

; ---------- 全部绘制 ----------
(defun C:DRAW_ALL_V2 ()
  (C:DRAW_REDLINE_V2)
  (C:DRAW_ROAD_V2)
  (command "_.ZOOM" "_E")
  (princ "\n=== 红线与道路全部绘制完成 ===")
  (princ "\n")
  (princ)
)

(princ "\n精确版脚本加载成功。")
(princ "\n命令：DRAW_REDLINE_V2 = 红线 | DRAW_ROAD_V2 = 道路 | DRAW_ALL_V2 = 全部")
(princ "\n")
(princ)