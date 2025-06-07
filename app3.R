# 加载包
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(pwr)#最普适的样本量估算包
library(pwr2)#或许用得上，Two-Factor ANOVA
library(PowerTOST)#交叉设计
library(longpower)#重复测量，多时间点
library(epiR)#流行病学研究板块
library(TrialSize)#Non-Inferiority / Equivalence / Superiority
library(gsDesign)#For Group Sequential
library(Superpower)# 大抵是没用了
library(powerSurvEpi)#生存分析
library(dplyr)
library(ggplot2)#绘图用
library(patchwork) #拼贴图用的
library(slickR)#轮播图
library(emayili)



library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translations.json")
i18n$set_translation_language("en")




#--------------- UI ---------------

ui <- fluidPage(
  usei18n(i18n),
  useShinyjs(),
  # ── 右上角语言切换器 ───────────────────────────────────────────
  tags$div(
    id    = "lang_switcher_fixed",
    style = "position: fixed; top: 10px; right: 20px; width: 130px; z-index: 3000;",
    selectInput(
      inputId  = "lang_top",
      label    = NULL,                   # 不显示 label，保持简洁
      choices  = c("中文" = "cn",
                   "English" = "en",
                   "日本語" = "jp"),
      selected = "en",                   # 先给个默认值；后面 server 会同步
      width    = "100%"
    )
  ),
  
  
  theme = shinytheme("flatly"),  # 主题
  
  tags$head(
    tags$style(HTML("
       /* 
         您的色卡：
         #001F34 (最深)
         #24435F
         #777F8C
         #A4B2BF (最浅)
       */

      /* ----未激活标签 (普通状态)---- */
      body .nav.nav-tabs > li > a {
        background-color: #A4B2BF !important;  /* 最浅色 */
        color: #ffffff !important;             /* 白色文字 */
        border-radius: 8px 8px 0 0;            /* 顶部圆角 (可改) */
        margin-right: 4px;
        border: 1px solid #A4B2BF !important;  /* 边框，可根据需要改为 #24435F */
        /* 渐变动画平滑过渡 */
        transition: background 0.5s, border 0.5s;
      }

      /* ----鼠标悬停 (hover)---- */
      body .nav.nav-tabs > li > a:hover {
        background: linear-gradient(135deg, #A4B2BF, #777F8C) !important;
        color: #ffffff !important;
      }

      /* ----当前激活(点击后)---- */
      body .nav.nav-tabs > li.active > a,
      body .nav.nav-tabs > li.active > a:focus,
      body .nav.nav-tabs > li.active > a:hover {
        /* 由最深至中等的颜色渐变 */
        background: linear-gradient(135deg, #001F34, #24435F) !important;
        color: #ffffff !important;
        border: 1px solid #001F34 !important; /* 边框可设置为深色 */
        border-bottom-color: #fff !important; 
        transition: background 0.5s, border 0.5s;
      }

      /* ----如果有 disabled 的标签 (不可点击)---- */
      body .nav.nav-tabs > li.disabled > a {
        background-color: #ddd !important;
        color: #999 !important;
        cursor: not-allowed !important;
      }

      /* ----选项卡内容区---- */
      body .tab-content {
        background-color: #ffffff;             /* 内容区背景 */
        color: #000;                           /* 内容文字颜色 */
        border: 1px solid #ddd;
        border-top-color: transparent !important;
        padding: 15px;
      }
      
      /* ----------------
  2. 固定footer在页面底部
---------------- */
  footer.fixed-footer {
    position: fixed;     /* 固定布局 */
      bottom: 0;           /* 靠下 */
      left: 0; right: 0;   /* 全宽 */
      z-index: 9999;       /* 保证置于上层，不被其它元素覆盖 */
      background-color: #001F34; /* 选自您色卡的深色 */
      color: #ffffff;
      text-align: center;
    padding: 10px;
    /* 可根据需要调整 height, margin 等 */
  }
    "))
  ),
  
  
  
  
  
  
  
  #全局主题配置
  # -- 新增：头部区域（Header） --
  tags$div(
    style = "
      background-color: #001F34;  /* 深色，取自色卡 */
      color: #ffffff;             /* 字体用白色 */
      padding: 15px;
      margin-bottom: 15px;
    ",
    h2(
     # "Sample Size Calculation Tool", 
      i18n$t("app_title"),
      style = "margin: 0;")
  ),
  

  

  
  # 主标题
  titlePanel(NULL),
  
  # 左侧为说明，右侧为主要功能区
  sidebarLayout(
    
    # 左侧侧边栏：
    div(
      id = "mySidebar",
    sidebarPanel(
      width = 3,  
      uiOutput("sidebar_instructions")
    )
    ),
    
    # 右侧主面板：包含了各个功能标签页
    mainPanel(
      width = 9 ,
      tabsetPanel(id = "myInnerTabs", 
                  
                  
                  
####-###-###-###-###-###-###-###-###
# 00. HOME模块----
####-###-###-###-###-###-###-###-###
        tabPanel(title =i18n$t("tab_home"),value = "Home",
                 # (1) 轮播图 (Carousel) - slickR输出
                 #    宽度、高度等可根据需要进行调整
                 slickROutput(
                   outputId = "my_slick", 
                   width = "60%", 
                   height = "400px"
                 ),
                 
                 br(),
                 
                 # (2) 英文介绍文字
                 p(i18n$t("home_intro_p1")),
                 
                 br(),
                 
                 # (3) 3个大按钮 (语言切换示例) - 宽100%、高100px，可按需调整
                 fluidRow(
                   
                   column(
                     width = 4,
                     align = "center",
                     actionButton(
                       inputId = "lang_en",
                       i18n$t("lang_en"),
                       #label = "English",
                       style = "width: 100%; height: 100px; color: white; 
                     background-color: #001F34; font-size: 20px;"
                     )
                   ),
                   column(
                     width = 4,
                     align = "center",
                     actionButton(
                       inputId = "lang_cn",
                       i18n$t("lang_cn"),
                       #label = "简体中文",
                       style = "width: 100%; height: 100px; color: white; 
                     background-color: #24435F; font-size: 20px;"
                     )
                   ),
                   column(
                     width = 4,
                     align = "center",
                     actionButton(
                       inputId = "lang_jp",
                       i18n$t("lang_jp"),
                       #label = "日本語",
                       style = "width: 100%; height: 100px; color: white; 
                     background-color: #777F8C; font-size: 20px;"
                     )
                   )
                 ),
                 
                 br(),
                 
                 # (4) 与 (5) 并排放置两个 panel (panel-primary)
                 fluidRow(
                   column(
                     width = 6,
                     div(
                       class = "panel panel-primary",
                       div(class = "panel-heading", 
                           h4(i18n$t("home_maintainers_h4"))),
                       div(
                         class = "panel-body",
                         p(i18n$t("home_maintainer_peng")),
                         p(i18n$t("home_maintainer_dai"))
                       )
                     )
                   ),
                   column(
                     width = 6,
                     div(
                       class = "panel panel-info",
                       div(class = "panel-heading", 
                           h4(i18n$t("home_updates_h4"))),
                       div(
                         class = "panel-body",
                         tags$ul(
                           tags$li(i18n$t("home_update_20250327")),
                           tags$li(i18n$t("home_update_20250306")),
                           tags$li(i18n$t("home_update_20250203")),
                           tags$li(i18n$t("home_update_more"))
                         )
                       )
                     )
                   )
                 ),
                 tags$hr(style = "border: 1px solid #ccc; margin: 20px 0;"),
                 # (6) 插入访客地图 (ClustrMaps)，调整大小
                 tags$div(
                   style = "display: flex; justify-content: center; align-items: center; width: 100%;", # 使用 flex 居中
                   tags$div(
                     style = "width: 300px; height: 300px;", # 设置具体宽度和高度
                     tags$script(
                       type = "text/javascript", 
                       id = "clstr_globe", 
                       src = "//clustrmaps.com/globe.js?d=_wwzrm5QcKku3LaDbZ8MCqOOOupE3-UUkH82NiyqTNg"
                     )
                   )
                 ),
                   
                 br(),
                 br(),
                 br(),
                 br()
          ),

####-###-###-###-###-###-###-###-###
# 0. 引导模块----
####-###-###-###-###-###-###-###-###
tabPanel(title = i18n$t("tab_guidance"),
         value ="Guidance",
         useShinyjs(),
         ##引导问卷----
         tabsetPanel(
           id = "guidance_subtabs",
           tabPanel(
             title  = i18n$t("wizard_questionnaire_tab"),
             value  = "wizard",
         fluidRow(
           # 左侧：分步骤问题
           column(4,
                  ## Step 1: 选择试验设计类型
                  div(id = "step1",
                     h4(i18n$t("wizard_step1_title")),
                      radioButtons("step1_design", label=NULL, inline=FALSE,
                                   choices = c(i18n$t("wizard_step1_single") = "single",
                                               i18n$t("wizard_step1_two") = "two",
                                               i18n$t("wizard_step1_multi") = "multi",
                                               i18n$t("wizard_step1_special") = "special")
                      ),
                      actionButton("step1_next", i18n$t("wizard_next"), class = "btn btn-primary")
                  ),
                  
                  ## Step 2: 根据 Step1 动态呈现的研究目标问题
                  hidden(div(id = "step2",
                             # 文本和选项由服务器根据 step1_design 动态生成
                             uiOutput("step2_ui"),
                             actionButton("step2_next", i18n$t("wizard_next"), class = "btn btn-primary")
                  )),
                  
                  ## Step 3: 根据前一步动态呈现的数据类型问题
                  hidden(div(id = "step3",
                             uiOutput("step3_ui"),
                             actionButton("step3_next", i18n$t("wizard_next"), class = "btn btn-primary")
                  )),
                  
                  ## Step 4: 若需要，呈现附加问题（如生存分析细节）
                  hidden(div(id = "step4",
                             uiOutput("step4_ui"),
                             actionButton("step4_finish", i18n$t("wizard_finish"), class = "btn btn-success")
                  ))
           ),
           
           # 右侧：说明面板（实时解释 & 最终结果）
           column(8,
                  tags$div(id="explanation_panel", 
                           style = "border: 1px solid #ddd; padding: 15px; background: #f9f9f9; border-radius: 5px;",
                           h4(i18n$t("wizard_panel_explanation")),
                           # 动态输出各步骤选择的解释文本
                           uiOutput("explanation_text")
                  ),
                  # 最终推荐结果区域（初始隐藏）
                  hidden(div(id = "result_panel",
                             h3(i18n$t("wizard_panel_recommend")),
                             htmlOutput("final_recommendation"),   # 最终推荐的方法与说明
                             br(),
                             actionButton("go_back_restart", i18n$t("wizard_restart"), icon = icon("redo")),
                             actionButton("wizard_go_to_tab", i18n$t("wizard_go_to_tab"), class = "btn btn-success")
                  ))
           )
         )
           ),
         tabPanel(
           title  = i18n$t("wizard_doc_tab"),
           value  = "guide_doc",
           # MathJax 自动生效；若想保险可再包一层 withMathJax()
           div(style = "padding:0 20px;",
               tags$iframe(
                 src = "指导文档.html",          # 相对 www/ 的路径
                 style = "width:100%;height:calc(100vh - 120px);border:none;"
               )
           )
         )
         )
),
        
####-###-###-###-###-###-###-###-###
# 1. T 检验类----
####-###-###-###-###-###-###-###-###

        tabPanel(i18n$t("tab_ttest"),   value = "T-test",
                 tabsetPanel(
                   ##===-===-===-===-===-===-===
                   ## 1.1 One-Sample t-Test----
                   ##===-===-===-===-===-===-===
                   tabPanel("One-Sample t-Test",
                            fluidRow(
                              column(4,
                                     # alpha
                                     numericInput(
                                       inputId = "t1_alpha",
                                       label   = span(
                                         i18n$t("label_significance"),
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Alpha represents the significance level, i.e., the probability of Type I error.');return false;"
                                         )
                                       ),
                                       value = 0.05,
                                       min   = 0,
                                       max   = 1,
                                       step  = 0.01
                                     ),
                                     
                                     # power
                                     numericInput(
                                       inputId = "t1_power",
                                       label   = span(
                                         i18n$t("label_power"),
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Statistical power is 1 - β, representing the probability of correctly rejecting the null hypothesis when the alternative is true.');return false;"
                                         )
                                       ),
                                       value = 0.8,
                                       min   = 0,
                                       max   = 1,
                                       step  = 0.01
                                     ),
                                     
                                     # delta
                                     numericInput(
                                       inputId = "t1_delta",
                                       label   = span(
                                         i18n$t("label_mean_diff"),
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('This is the expected difference between the true mean and the null hypothesis mean.');return false;"
                                         )
                                       ),
                                       value = 0.5,
                                       step  = 0.1
                                     ),
                                     
                                     # sd
                                     numericInput(
                                       inputId = "t1_sd",
                                       label   = span(
                                         i18n$t("label_sd"),
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Standard deviation (σ) is required to compute Cohen\\'s d = (delta / sd).');return false;"
                                         )
                                       ),
                                       value = 1,
                                       step  = 0.1
                                     ),
                                     
                                     # alternative
                                     selectInput(
                                       inputId = "t1_alternative",
                                       label   = span(
                                         i18n$t("label_alt_hypothesis"),
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Determines the direction of the test:\\n\\\"two.sided\\\" tests both directions,\\n\\\"less\\\" or \\\"greater\\\" test one-sided.');return false;"
                                         )
                                       ),
                                       choices  = c("two.sided", "less", "greater"),
                                       selected = "two.sided"
                                     ),
                                     
                                     selectInput(
                                       inputId = "t1_x_variable",
                                       label   = span(
                                         i18n$t("label_select_x"),
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Choose the variable to display on the x-axis of the power curve: \\n\\\"Sample Size\\\" or \\\"Effect Size\\\".');return false;"
                                         )
                                       ),
                                       choices  = c("Sample Size" = "sample_size", "Effect Size" = "effect_size"),
                                       selected = "sample_size"
                                     ),
                                     
                                     br(),
                                     actionBttn(
                                       inputId = "t1_calc",
                                       label = i18n$t("btn_calc"),
                                       style = "gradient",
                                       color = "primary"
                                     ),
                                     br()
                              ),
                              column(8,
                                     verbatimTextOutput("t1_result"),
                                     tags$div(
                                       style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9;",
                                       h4(i18n$t("title_power_curve"), style = "margin-bottom: 20px; font-family: Arial, Helvetica, sans-serif;"),
                                       plotOutput(outputId = "t1_power_curve", height = "400px") # Output plot here
                                     )
                              )
                            )
                   ),
                   
                   ##===-===-===-===-===-===-===
                   ## 1.2 Paired t-Test----
                   ##===-===-===-===-===-===-===
                   tabPanel("Paired t-Test",
                            fluidRow(
                              column(4,
                                     # alpha
                                     numericInput(
                                       inputId = "paired_alpha",
                                       label   = span(
                                         "Significance Level (alpha)",
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Alpha represents the significance level, i.e., the probability of Type I error.');return false;"
                                         )
                                       ),
                                       value = 0.05,
                                       min   = 0,
                                       max   = 1,
                                       step  = 0.01
                                     ),
                                     
                                     # power
                                     numericInput(
                                       inputId = "paired_power",
                                       label   = span(
                                         "Statistical Power (1 - β)",
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Statistical power is 1 - β, representing the probability of correctly rejecting the null hypothesis when the alternative is true.');return false;"
                                         )
                                       ),
                                       value = 0.8,
                                       min   = 0,
                                       max   = 1,
                                       step  = 0.01
                                     ),
                                     
                                     # effect size (cohen's d)
                                     numericInput(
                                       inputId = "paired_d",
                                       label   = span(
                                         "Effect Size (Cohen's d)",
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('In a paired t-test, effect size is typically (MeanDiff / SD_of_diff).');return false;"
                                         )
                                       ),
                                       value = 0.5,
                                       step  = 0.1
                                     ),
                                     
                                     # alternative
                                     selectInput(
                                       inputId = "paired_alternative",
                                       label   = span(
                                         "Alternative Hypothesis",
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Determines the direction of the test:\\n\\\"two.sided\\\" tests both directions,\\n\\\"less\\\" or \\\"greater\\\" test one-sided.');return false;"
                                         )
                                       ),
                                       choices  = c("two.sided", "less", "greater"),
                                       selected = "two.sided"
                                     ),
                                     
                                     br(),
                                     actionBttn(
                                       inputId = "paired_calc",
                                       label = i18n$t("btn_calc"),
                                       style = "gradient",
                                       color = "primary"
                                     )
                              ),
                              column(8,
                                     # 显示计算结果
                                     verbatimTextOutput("paired_result"),
                                     # 用来显示两张拼贴的图
                                     tags$div(
                                       style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9; margin-top:14px;",
                                       h4("Power Curves", style = "margin-bottom: 20px; font-family: Arial, Helvetica, sans-serif;"),
                                       plotOutput("paired_power_curves", height = "400px")  
                                     )
                              )
                            )
                   ),
                   
                   ##===-===-===-===-===-===-===
                   ## 1.3 Two-Independent-Sample t-Test----
                   ##===-===-===-===-===-===-===
                   tabPanel("Two-Independent-Sample t-Test",
                            fluidRow(
                              column(4,
                                     # alpha
                                     numericInput(
                                       inputId = "ind_two_alpha",
                                       label   = span(
                                         "Significance Level (alpha)",
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Alpha represents the significance level, i.e., the probability of Type I error.');return false;"
                                         )
                                       ),
                                       value = 0.05,
                                       min   = 0,
                                       max   = 1,
                                       step  = 0.01
                                     ),
                                     
                                     # power
                                     numericInput(
                                       inputId = "ind_two_power",
                                       label   = span(
                                         "Statistical Power (1 - β)",
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Statistical power is 1 - β, representing the probability of correctly rejecting the null hypothesis when the alternative is true.');return false;"
                                         )
                                       ),
                                       value = 0.8,
                                       min   = 0,
                                       max   = 1,
                                       step  = 0.01
                                     ),
                                     
                                     # effect size
                                     numericInput(
                                       inputId = "ind_two_d",
                                       label   = span(
                                         "Effect Size (Cohen's d)",
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('In a two-sample t-test, effect size is typically (Mean1 - Mean2)/SD_pooled.');return false;"
                                         )
                                       ),
                                       value = 0.4,
                                       step  = 0.1
                                     ),
                                     
                                     # alternative
                                     selectInput(
                                       inputId = "ind_two_alternative",
                                       label   = span(
                                         "Alternative Hypothesis",
                                         style = "font-family: Arial, Helvetica, sans-serif;",
                                         tags$a(
                                           tags$i(class='fa fa-question-circle'),
                                           href = "#",
                                           onclick="alert('Determines the direction of the test:\\n\\\"two.sided\\\" tests both directions,\\n\\\"less\\\" or \\\"greater\\\" test one-sided.');return false;"
                                         )
                                       ),
                                       choices  = c("two.sided", "less", "greater"),
                                       selected = "two.sided"
                                     ),
                                     
                                     br(),
                                     actionBttn(
                                       inputId = "ind_two_calc",
                                       label = i18n$t("btn_calc"),
                                       style = "gradient",
                                       color = "primary"
                                     )
                              ),
                              column(8,
                                     verbatimTextOutput("ind_two_result"),
                                     tags$div(
                                       style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9; margin-top:14px;",
                                       h4("Power Curves", style = "margin-bottom: 20px; font-family: Arial, Helvetica, sans-serif;"),
                                       plotOutput("ind_two_power_curves", height = "400px")
                                     )
                              )
                            )
                   )
                 )
        ),
        
####-###-###-###-###-###-###-###-###
# 2. 方差分析类----
####-###-###-###-###-###-###-###-###
        
        
        tabPanel(title = i18n$t("tab_anova"),   value = "ANOVA", 
                 tabsetPanel(
                   ##===-===-===-===-===-===-===
                   ## 2.1 Two-Period Crossover----
                   ##===-===-===-===-===-===-===
                   tabPanel("Two-Period Crossover",
                            fluidRow(
                              column(
                                width = 4,
                                h3("Two-Period Crossover (2×2)"),
                                
                                numericInput(
                                  inputId = "crossover_alpha",
                                  label = span(
                                    "Significance Level (alpha)",
                                    style = "font-family: Arial, Helvetica, sans-serif; margin-right: 5px;",
                                    tags$a(
                                      tags$i(class = "fa fa-question-circle"),
                                      href = "#",
                                      onclick = "alert('Alpha is the probability of making a Type I error — i.e., rejecting a true null hypothesis. Commonly set at 0.05.'); return false;"
                                    )
                                  ),
                                  value = 0.05,
                                  step  = 0.01
                                ),
                                
                                numericInput(
                                  inputId = "crossover_power",
                                  label = span(
                                    "Target Power",
                                    style = "font-family: Arial, Helvetica, sans-serif; margin-right: 5px;",
                                    tags$a(
                                      tags$i(class = "fa fa-question-circle"),
                                      href = "#",
                                      onclick = "alert('Target power refers to the probability of detecting a true effect (1 - β).'); return false;"
                                    )
                                  ),
                                  value = 0.80,
                                  step  = 0.01
                                ),
                                
                                numericInput(
                                  inputId = "crossover_cv",
                                  label = span(
                                    "Coefficient of Variation (CV)",
                                    style = "font-family: Arial, Helvetica, sans-serif; margin-right: 5px;",
                                    tags$a(
                                      tags$i(class = "fa fa-question-circle"),
                                      href = "#",
                                      onclick = "alert('CV represents coefficient of variation, i.e., within-subject variability in a crossover design.'); return false;"
                                    )
                                  ),
                                  value = 0.3,
                                  step  = 0.01
                                ),
                                
                                numericInput(
                                  inputId = "crossover_ratio",
                                  label = span(
                                    "θ0 (True Ratio)",
                                    style = "font-family: Arial, Helvetica, sans-serif; margin-right: 5px;",
                                    tags$a(
                                      tags$i(class = "fa fa-question-circle"),
                                      href = "#",
                                      onclick = "alert('θ0 is the assumed true ratio of test/reference means. Often close to 1.0 in bioequivalence studies.'); return false;"
                                    )
                                  ),
                                  value = 0.95,
                                  step  = 0.01
                                ),
                                
                                actionButton(
                                  inputId = "crossover_calc",
                                  label   = "Calculate"
                                )
                              ),
                              column(
                                width = 8,
                                verbatimTextOutput("crossover_result"),
                                tags$div(
                                  style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9;",
                                  h4("Power Curve", style = "margin-bottom: 20px; font-family: Arial, Helvetica, sans-serif;"),
                                  plotOutput(outputId = "crossover_power_curves", height = "400px") # Output plot here
                                )
                                
                              )
                            )
                   ),
                   ##===-===-===-===-===-===-===
                   ## 2.2 One-Way ANOVA----
                   ##===-===-===-===-===-===-===
                   tabPanel("One-Way ANOVA",
                            
                            fluidRow(
                              column(
                                width = 4,
                                h3("One-Way ANOVA"),
                                
                                numericInput(
                                  inputId = "anova1_alpha",
                                  label = span(
                                    "Significance Level (alpha)",
                                    style = "font-family: Arial, Helvetica, sans-serif; margin-right: 5px;",
                                    tags$a(
                                      tags$i(class = "fa fa-question-circle"),
                                      href = "#",
                                      onclick = "alert('Probability of Type I error. Typically 0.05.'); return false;"
                                    )
                                  ),
                                  value = 0.05,
                                  step  = 0.01
                                ),
                                
                                numericInput(
                                  inputId = "anova1_power",
                                  label = span(
                                    "Target Power",
                                    style = "font-family: Arial, Helvetica, sans-serif; margin-right: 5px;",
                                    tags$a(
                                      tags$i(class = "fa fa-question-circle"),
                                      href = "#",
                                      onclick = "alert('Desired probability of detecting a difference if it truly exists. Often 0.8 or 0.9.'); return false;"
                                    )
                                  ),
                                  value = 0.80,
                                  step  = 0.01
                                ),
                                
                                numericInput(
                                  inputId = "anova1_f",
                                  label = span(
                                    "Cohen's f",
                                    style = "font-family: Arial, Helvetica, sans-serif; margin-right: 5px;",
                                    tags$a(
                                      tags$i(class = "fa fa-question-circle"),
                                      href = "#",
                                      onclick = "alert('Effect size measure for ANOVA. Typical guide: 0.1=small, 0.25=medium, 0.4=large.'); return false;"
                                    )
                                  ),
                                  value = 0.25,
                                  step  = 0.01
                                ),
                                
                                numericInput(
                                  inputId = "anova1_k",
                                  label = span(
                                    "Number of Groups (k)",
                                    style = "font-family: Arial, Helvetica, sans-serif; margin-right: 5px;",
                                    tags$a(
                                      tags$i(class = "fa fa-question-circle"),
                                      href = "#",
                                      onclick = "alert('How many groups/levels are in the One-Way ANOVA? Must be >=2.'); return false;"
                                    )
                                  ),
                                  value = 3,
                                  step  = 1,
                                  min   = 2
                                ),
                                
                                actionButton(
                                  inputId = "anova1_calc",
                                  label   = "Calculate"
                                )
                              ),
                              column(
                                width = 8,
                                verbatimTextOutput("anova1_result"),
                                tags$div(
                                  style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9;",
                                  h4("Power Curve", style = "margin-bottom: 20px; font-family: Arial, Helvetica, sans-serif;"),
                                  plotOutput(outputId = "anova1_power_curves", height = "400px") # Output plot here
                                )
                              )
                            )
                              
                   ),
                   ##===-===-===-===-===-===-===
                   ## 2.3 Two-Factor ANOVA----
                   ##===-===-===-===-===-===-===
                   tabPanel("Two-Factor ANOVA",
                            
                            fluidRow(
                              column(
                                width = 4,
                                h3("Two-Factor ANOVA"),
                                
                                numericInput("a", span(
                                  "Number of Levels for Factor A (a)",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Specify how many levels (groups) Factor A has. This represents the number of categories or conditions in the first factor. Must be at least 2.'); return false;"
                                  )
                                ), 2, min = 2, step = 1),
                                
                                numericInput("b", span(
                                  "Number of Levels for Factor B (b)",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Specify how many levels (groups) Factor B has. This represents the number of categories or conditions in the second factor. Must be at least 2.'); return false;"
                                  )
                                ), 3, min = 2, step = 1),
                                
                                numericInput("alpha", span(
                                  "Significance Level (alpha)",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Alpha represents the probability of Type I error, typically set at 0.05.'); return false;"
                                  )
                                ), 0.05, step = 0.01),
                                
                                numericInput("fA", span(
                                  "Effect Size for Factor A (f.A)",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Cohen\\'s f for Factor A. Indicates the expected effect size for the main effect of Factor A.&#10;Typical benchmarks: 0.1 = small, 0.25 = medium, 0.4 = large.'); return false;"
                                  )
                                ), 0.25, step = 0.01),
                                
                                numericInput("fB", span(
                                  "Effect Size for Factor B (f.B)",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Cohen\\'s f for Factor B. Indicates the expected effect size for the main effect of Factor B.&#10;Typical benchmarks: 0.1 = small, 0.25 = medium, 0.4 = large.'); return false;"
                                  )
                                ), 0.25, step = 0.01),
                                
                                
                                numericInput("target_power", span(
                                  "Target Power",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Desired probability (1 - β) to detect an effect if it truly exists. Common choices are 0.8 or 0.9.'); return false;"
                                  )
                                ), 0.80, step = 0.01),
                                
                                actionButton("calc_power", "Calculate")
                              ),
                              column(
                                width = 8,
                                verbatimTextOutput("anova2_result")
                                
                              )
                            )
                            
                   ),
                   ##===-===-===-===-===-===-===
                   ## 2.4 Repeated-Measures ANOVA----
                   ##===-===-===-===-===-===-===
                   tabPanel("Repeated-Measures ANOVA",
                          
                            fluidRow(
                              column(
                                width = 4,
                                h3("Repeated-Measures ANOVA"),
                                
                                numericInput("arOneRho", span(
                                  "AR(1) Correlation Coefficient (rho)",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Assumed AR(1) correlation between repeated measurements.Higher values imply stronger autocorrelation across time points.'); return false;"
                                  )
                                ), value = 0.76, step = 0.01),
                                
                                numericInput("arOneSD", span(
                                  "Standard Deviation per Measurement (σ)",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Standard deviation of the repeated outcome at each time point.Used to calculate variability within subjects.'); return false;"
                                  )
                                ), value = 25, step = 1),
                                
                                numericInput("arOneT", span(
                                  "Number of Repeated Measurements (T)",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Number of time points per subject.Each subject is measured T times. Must be ≥ 2.'); return false;"
                                  )
                                ), value = 6, min = 2, step = 1),
                                
                                numericInput("arOneDelta", span(
                                  "Mean Difference Between Groups (Δ)",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Expected average difference between treatment groups.Used to calculate the effect size.'); return false;"
                                  )
                                ), value = 14.3, step = 0.1),
                                
                                numericInput("arOneAlpha", span(
                                  "Significance Level (alpha)",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Type I error rate.Common values are 0.05 or 0.01.'); return false;"
                                  )
                                ), value = 0.05, step = 0.01),
                                
                                numericInput("arOnePower", span(
                                  "Target Power",
                                  tags$a(
                                    tags$i(class = "fa fa-question-circle"),
                                    href = "#",
                                    onclick = "alert('Desired probability of correctly rejecting the null hypothesis when a true effect exists.Typically 0.8 or 0.9.'); return false;"
                                  )
                                ), value = 0.90, step = 0.01),
                                
                                
                                
                                actionButton("calcArOneBtn", "Calculate")
                                
                              ),
                              column(
                                width = 8,
                                verbatimTextOutput("arOneResult")
                              )
                            )
                            
                            
                   )
                 )
        ),
        
####-###-###-###-###-###-###-###-###
# 3. 比例检验类----
####-###-###-###-###-###-###-###-###
        tabPanel("Proportion Tests",
                 tabsetPanel(
                   ##===-===-===-===-===-===-===
                   ## 3.1 One-Sample Proportion Test----
                   ##===-===-===-===-===-===-===
                   tabPanel("One-Sample Proportion Test",
                            fluidRow(
                              column(4,
                                     
                                     numericInput(
                                       inputId = "prop_one_alpha",
                                       label = span("Significance Level (alpha)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Probability of Type I error. Common default is 0.05.'); return false;"
                                                    )
                                       ),
                                       value = 0.05, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "prop_one_power",
                                       label = span("Statistical Power (1 - β)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Probability of correctly rejecting the null when the alternative is true.'); return false;"
                                                    )
                                       ),
                                       value = 0.80, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "prop_one_p0",
                                       label = span("Null Hypothesis Proportion (p0)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Proportion under the null hypothesis. For example, 0.5.'); return false;"
                                                    )
                                       ),
                                       value = 0.5, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "prop_one_p1",
                                       label = span("Alternative Proportion (p1)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Expected true proportion under the alternative hypothesis.'); return false;"
                                                    )
                                       ),
                                       value = 0.6, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     selectInput(
                                       inputId = "prop_one_alternative",
                                       label = span("Alternative Hypothesis",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Choose two.sided for a bidirectional test, or less/greater for one-sided.'); return false;"
                                                    )
                                       ),
                                       choices = c("two.sided", "less", "greater"),
                                       selected = "two.sided"
                                     ),
                                     
                                     
                                     # 触发计算
                                     actionBttn(
                                       inputId = "prop_one_calc",
                                       label = "Calculate Sample Size",
                                       style = "gradient",
                                       color = "primary"
                                     )
                              ),
                              column(8,
                                     verbatimTextOutput("prop_one_result"),
                                     tags$div(
                                       style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9; margin-top:14px;",
                                       h4("Power Curves"),
                                       plotOutput("prop_one_power_curve", height = "400px")
                                     )
                              )
                            )
                   ),
                   
                   
                   ##===-===-===-===-===-===-===
                   ## 3.2 Paired Proportion Comparison----
                   ##===-===-===-===-===-===-===
                   tabPanel("Paired Proportion Comparison",
                            fluidRow(
                              column(4,
                                     
                                     numericInput(
                                       inputId = "prop_paired_alpha",
                                       label = span("Significance Level (alpha)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Probability of Type I error. Often 0.05.'); return false;"
                                                    )
                                       ),
                                       value = 0.05, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "prop_paired_power",
                                       label = span("Statistical Power (1 - β)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Power to detect a true difference in paired proportions.'); return false;"
                                                    )
                                       ),
                                       value = 0.80, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "prop_paired_p1",
                                       label = span("Proportion 1 (Approx.)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Estimated proportion in condition 1.'); return false;"
                                                    )
                                       ),
                                       value = 0.5, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "prop_paired_p2",
                                       label = span("Proportion 2 (Approx.)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Estimated proportion in condition 2.'); return false;"
                                                    )
                                       ),
                                       value = 0.6, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     
                                     
                                     actionBttn(
                                       inputId = "prop_paired_calc",
                                       label = "Calculate Sample Size",
                                       style = "gradient",
                                       color = "primary"
                                     )
                              ),
                              column(8,
                                     verbatimTextOutput("prop_paired_result"),
                                     
                              )
                            )
                   ),
                   ##===-===-===-===-===-===-===
                   ## 3.3 Two-Independent Proportions Comparison----
                   ##===-===-===-===-===-===-===
                   tabPanel("Two-Independent Proportions Comparison",
                            fluidRow(
                              column(4,
                                     
                                     numericInput(
                                       inputId = "prop_two_alpha",
                                       label = span("Significance Level (alpha)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Probability of Type I error. Often 0.05.'); return false;"
                                                    )
                                       ),
                                       value = 0.05, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "prop_two_power",
                                       label = span("Statistical Power (1 - β)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Desired power to detect a difference between two independent proportions.'); return false;"
                                                    )
                                       ),
                                       value = 0.80, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "prop_two_p1",
                                       label = span("Proportion 1 (p1)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Expected proportion in group 1.'); return false;"
                                                    )
                                       ),
                                       value = 0.3, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "prop_two_p2",
                                       label = span("Proportion 2 (p2)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Expected proportion in group 2.'); return false;"
                                                    )
                                       ),
                                       value = 0.5, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     selectInput(
                                       inputId = "prop_two_alternative",
                                       label = span("Alternative Hypothesis",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Choose two.sided for general difference, or one-sided alternatives.'); return false;"
                                                    )
                                       ),
                                       choices = c("two.sided", "less", "greater"),
                                       selected = "two.sided"
                                     ),
                                     
                                     
                                     actionBttn(
                                       inputId = "prop_two_calc",
                                       label = "Calculate Sample Size",
                                       style = "gradient",
                                       color = "primary"
                                     )
                              ),
                              
                              column(8,
                                     verbatimTextOutput("prop_two_result"),
                                     tags$div(
                                       style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9; margin-top:14px;",
                                       h4("Power Curves"),
                                       plotOutput("prop_two_power_curve", height = "400px")
                                     )
                              )
                            )
                   ),
                   ##===-===-===-===-===-===-===
                   ## 3.4 Multiple Proportions (Completely Randomized)----
                   ##===-===-===-===-===-===-===
                   tabPanel("Multiple Proportions (Completely Randomized)",
                            fluidRow(
                              column(4,
                                     numericInput(
                                       inputId = "prop_multi_alpha",
                                       label = span("Significance Level (alpha)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Probability of Type I error. Usually set at 0.05.'); return false;"
                                                    )
                                       ),
                                       value = 0.05, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "prop_multi_power",
                                       label = span("Statistical Power (1 - β)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Desired power for detecting a difference across multiple proportions.'); return false;"
                                                    )
                                       ),
                                       value = 0.80, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput("prop_multi_grp1", span(
                                       "Group1 proportion",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected proportion for group 1.'); return false;")
                                     ), 0.2, min=0, max=1, step=0.01),
                                     
                                     numericInput("prop_multi_grp2", span(
                                       "Group2 proportion",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected proportion for group 2.'); return false;")
                                     ), 0.5, min=0, max=1, step=0.01),
                                     
                                     numericInput("prop_multi_grp3", span(
                                       "Group3 proportion",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected proportion for group 3.'); return false;")
                                     ), 0.3, min=0, max=1, step=0.01),
                                     
                                     
                                     actionBttn(
                                       inputId = "prop_multi_calc",
                                       label   = "Calculate Sample Size",
                                       style   = "gradient",
                                       color   = "primary"
                                     )
                              ),
                              column(8,
                                     verbatimTextOutput("prop_multi_result"),
                                     tags$div(
                                       style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9; margin-top:14px;",
                                       h4("Power Curves"),
                                       plotOutput("prop_multi_power_curve", height = "400px")
                                     )
                              )
                            )
                   )
                 )
        ),
        
####-###-###-###-###-###-###-###-###
# 4. 相关系数检验类----
####-###-###-###-###-###-###-###-###
        tabPanel("Correlation Tests",
                 tabsetPanel(
                   ##===-===-===-===-===-===-===
                   ## 4.1 Single Correlation----
                   ##===-===-===-===-===-===-===
                   tabPanel("Single Correlation",
                            fluidRow(
                              column(4,
                                     numericInput(
                                       inputId = "corr_one_alpha",
                                       label = span("Significance Level (alpha)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Probability of making a Type I error. Common default is 0.05.'); return false;")
                                       ),
                                       value = 0.05, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "corr_one_power",
                                       label = span("Statistical Power (1 - β)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Probability of correctly detecting a true non-zero correlation.'); return false;")
                                       ),
                                       value = 0.80, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "corr_one_r",
                                       label = span("Correlation (r)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Expected Pearson correlation coefficient under the alternative hypothesis.'); return false;")
                                       ),
                                       value = 0.3, min = -1, max = 1, step = 0.01
                                     ),
                                     
                                     selectInput(
                                       inputId = "corr_one_alternative",
                                       label = span("Alternative Hypothesis",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Choose two.sided to test r ≠ 0, or one-sided for directional alternatives.'); return false;")
                                       ),
                                       choices = c("two.sided", "less", "greater"),
                                       selected = "two.sided"
                                     ),
                                     
                                     
                                     # 触发计算
                                     actionBttn(
                                       inputId = "corr_one_calc",
                                       label = "Calculate Sample Size",
                                       style = "gradient",
                                       color = "primary"
                                     )
                              ),
                              
                              column(8,
                                     verbatimTextOutput("corr_one_result"),
                                     tags$div(
                                       style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; 
                 background-color: #f9f9f9; margin-top:14px;",
                                       h4("Power Curves"),
                                       plotOutput("corr_one_power_curve", height = "400px")
                                     )
                              )
                            )
                   ),
                   
                   
                   ##===-===-===-===-===-===-===
                   ## 4.2 Two Correlations Comparison----
                   ##===-===-===-===-===-===-===
                   tabPanel("Two Correlations Comparison",
                            fluidRow(
                              column(4,
                                     numericInput(
                                       inputId = "corr_two_alpha",
                                       label = span("Significance Level (alpha)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Probability of making a Type I error. Common default is 0.05.'); return false;")
                                       ),
                                       value = 0.05, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "corr_two_power",
                                       label = span("Statistical Power (1 - β)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Probability of correctly detecting a difference between the two correlations.'); return false;")
                                       ),
                                       value = 0.80, min = 0, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "corr_two_r1",
                                       label = span("Correlation 1 (r1)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Expected correlation coefficient in the first group.'); return false;")
                                       ),
                                       value = 0.3, min = -1, max = 1, step = 0.01
                                     ),
                                     
                                     numericInput(
                                       inputId = "corr_two_r2",
                                       label = span("Correlation 2 (r2)",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Expected correlation coefficient in the second group.'); return false;")
                                       ),
                                       value = 0.5, min = -1, max = 1, step = 0.01
                                     ),
                                     
                                     selectInput(
                                       inputId = "corr_two_alternative",
                                       label = span("Alternative Hypothesis",
                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                           onclick = "alert('Choose two.sided to test r1 ≠ r2, or one-sided for directional hypotheses.'); return false;")
                                       ),
                                       choices = c("two.sided", "less", "greater"),
                                       selected = "two.sided"
                                     ),
                                     
                                     
                                     actionBttn(
                                       inputId = "corr_two_calc",
                                       label   = "Calculate Sample Size",
                                       style   = "gradient",
                                       color   = "primary"
                                     )
                              ),
                              
                              column(8,
                                     verbatimTextOutput("corr_two_result"),
                                     tags$div(
                                       style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; 
                 background-color: #f9f9f9; margin-top:14px;",
                                       h4("Power Curves"),
                                       plotOutput("corr_two_power_curve", height = "400px")
                                     )
                              )
                            )
                   )
                 )
        ),
        
####-###-###-###-###-###-###-###-###
# 5. 流行病学研究类----
####-###-###-###-###-###-###-###-###
        tabPanel("Epidemiological Studies",
                 tabsetPanel(
                   ##===-===-===-===-===-===-===
                   ## 5.1 Case-Control (epiR) - UI----
                   ##===-===-===-===-===-===-===
                   tabPanel("Case-Control (epiR)",
                            fluidRow(
                              column(4,
                                     numericInput("epir_cc_alpha", span(
                                       "Significance Level (α)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Probability of Type I error. Common value is 0.05 for 95% confidence.'); return false;")
                                     ), value = 0.05, min = 0.0001, max = 0.5, step = 0.01),
                                     
                                     numericInput("epir_cc_power", span(
                                       "Power (1 - β)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Desired power to detect a true odds ratio difference between groups.'); return false;")
                                     ), value = 0.80, min = 0, max = 1, step = 0.01),
                                     
                                     numericInput("epir_cc_or", span(
                                       "Odds Ratio (OR)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Assumed odds ratio of exposure between cases and controls. OR > 1 means higher odds in exposed.'); return false;")
                                     ), value = 2, min = 1.01, max = 100, step = 0.1),
                                     
                                     numericInput("epir_cc_p0", span(
                                       "Exposure in Controls (p0)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Proportion of controls expected to have the exposure.'); return false;")
                                     ), value = 0.3, min = 0, max = 1, step = 0.01),
                                     
                                     numericInput("epir_cc_r", span(
                                       "Case:Control ratio",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Ratio of the number of cases to the number of controls. Often 1.'); return false;")
                                     ), value = 1, min = 0.01, max = 10, step = 0.1),
                                     
                                     
                                     actionBttn(
                                       inputId = "epir_cc_calc",
                                       label   = "Calculate",
                                       style   = "gradient",
                                       color   = "primary"
                                     )
                              ),
                              column(8,
                                     verbatimTextOutput("epir_cc_result"),
                                     tags$div(
                                       style = "
            border:1px solid #ddd; 
            padding:10px; 
            border-radius:5px; 
            background:#f9f9f9; 
            margin-top:14px;",
                                       h4("Power Curves"),
                                       plotOutput("epir_cc_power_plot", height="400px")
                                     )
                              )
                            )
                   ),
                   
                   
                   ##===-===-===-===-===-===-===
                   ## 5.2 Cohort (RR) (epiR) - UI----
                   ##===-===-===-===-===-===-===
                   tabPanel("Cohort (RR) (epiR)",
                            fluidRow(
                              column(4,
                                     numericInput("epir_cohort_rr_alpha", span(
                                       "Significance Level (α)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Probability of Type I error. Typical value is 0.05.'); return false;")
                                     ), value = 0.05, min = 0.0001, max = 0.5, step = 0.01),
                                     
                                     numericInput("epir_cohort_rr_power", span(
                                       "Power (1 - β)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Desired power to detect a difference in risk between exposed and non-exposed groups.'); return false;")
                                     ), value = 0.80, min = 0, max = 1, step = 0.01),
                                     
                                     numericInput("epir_cohort_rr_p0", span(
                                       "Baseline Risk (p0)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected incidence in the non-exposed group.'); return false;")
                                     ), value = 0.1, min = 0, max = 1, step = 0.01),
                                     
                                     numericInput("epir_cohort_rr_val", span(
                                       "Relative Risk (RR)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Assumed relative risk (incidence in exposed / incidence in non-exposed).'); return false;")
                                     ), value = 2, min = 1.01, max = 10, step = 0.1),
                                     
                                     numericInput("epir_cohort_rr_ratio", span(
                                       "Exposed : NonExp ratio",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Sample size ratio of exposed to non-exposed individuals.'); return false;")
                                     ), value = 1, min = 0.01, max = 10, step = 0.1),
                                     
                                     
                                     actionBttn(
                                       inputId = "epir_cohort_rr_calc",
                                       label   = "Calculate",
                                       style   = "gradient",
                                       color   = "primary"
                                     )
                              ),
                              column(8,
                                     verbatimTextOutput("epir_cohort_rr_result"),
                                     tags$div(
                                       style = "
            border:1px solid #ddd; 
            padding:10px; 
            border-radius:5px; 
            background:#f9f9f9; 
            margin-top:14px;",
                                       h4("Power Curves"),
                                       plotOutput("epir_cohort_rr_plot", height="400px")
                                     )
                              )
                            )
                   ),
                   
                   
                   ##===-===-===-===-===-===-===
                   ## 5.3 Cohort (p1, p2) (epiR) - UI----
                   ##===-===-===-===-===-===-===
                   tabPanel("Cohort (p1,p2) (epiR)",
                            fluidRow(
                              column(4,
                                     numericInput("epir_cohort_p1p2_alpha", span(
                                       "Significance Level (α)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Probability of Type I error. Default is 0.05.'); return false;")
                                     ), value = 0.05, min = 0.0001, max = 0.5, step = 0.01),
                                     
                                     numericInput("epir_cohort_p1p2_power", span(
                                       "Power (1 - β)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Desired power to detect a difference in incidence between groups.'); return false;")
                                     ), value = 0.80, min = 0, max = 1, step = 0.01),
                                     
                                     numericInput("epir_cohort_p1p2_p1", span(
                                       "Incidence Risk in Exposed Group (P1)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected risk (incidence proportion) in the exposed group.'); return false;")
                                     ), value = 0.2, min = 0, max = 1, step = 0.01),
                                     
                                     numericInput("epir_cohort_p1p2_p2", span(
                                       "Incidence Risk in Non-Exposed Group (P2)",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected risk (incidence proportion) in the non-exposed group.'); return false;")
                                     ), value = 0.1, min = 0, max = 1, step = 0.01),
                                     
                                     numericInput("epir_cohort_p1p2_ratio", span(
                                       "Exposed : NonExp ratio",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Sample size ratio of exposed to non-exposed individuals.'); return false;")
                                     ), value = 1, min = 0.01, max = 10, step = 0.1),
                                     
                                     
                                     actionBttn(
                                       inputId = "epir_cohort_p1p2_calc",
                                       label   = "Calculate",
                                       style   = "gradient",
                                       color   = "primary"
                                     )
                              ),
                              column(8,
                                     verbatimTextOutput("epir_cohort_p1p2_result"),
                                     tags$div(
                                       style = "
            border:1px solid #ddd; 
            padding:10px; 
            border-radius:5px; 
            background:#f9f9f9; 
            margin-top:14px;",
                                       h4("Power Curves"),
                                       plotOutput("epir_cohort_p1p2_plot", height="400px")
                                     )
                              )
                            )
                   )
                 )
        ),
        
####-###-###-###-###-###-###-###-###
# 6. 非劣效 / 等效 / 优效性试验类----
####-###-###-###-###-###-###-###-###

        ##===-===-===-===-===-===-===
        ##  6.1 (A) Non-Inferiority - UI----
        ##===-===-===-===-===-===-===
        
        tabPanel("Non-Inferiority",
                 
                 tabsetPanel(
                   ###────────────────────────────────────
                   ### A.1 Two Means (Difference) ----
                   ###────────────────────────────────────
                   tabPanel("Two Means (Difference)",
                            fluidRow(
                              column(4,
                                     numericInput("ni_means_alpha", span(
                                       "Significance Level α (One side):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('One-sided significance level used for hypothesis testing. Typical value is 0.025 for non-inferiority trials.'); return false;")
                                     ), value=0.025, min=0, max=0.1, step=0.001),
                                     
                                     numericInput("ni_means_power", span(
                                       "Power (1-β):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Desired power of the test, i.e., probability of correctly detecting non-inferiority if it exists.'); return false;")
                                     ), value=0.80, min=0, max=1, step=0.01),
                                     
                                     numericInput("ni_means_sigma", span(
                                       "Pooled Std Dev (σ):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Pooled standard deviation used to estimate variability when calculating sample size.'); return false;")
                                     ), value=1.5, min=0.0001),
                                     
                                     numericInput("ni_means_ratio", span(
                                       "Allocation ratio (n1/n2):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Ratio of sample size in treatment group (n1) to control group (n2). Usually set to 1 for equal allocation.'); return false;")
                                     ), value=1),
                                     
                                     numericInput("ni_means_margin", span(
                                       "NI Margin (delta):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Non-inferiority margin — the smallest clinically acceptable difference where new treatment is still considered non-inferior.'); return false;")
                                     ), value=0.5),
                                     
                                     numericInput("ni_means_diff", span(
                                       "True difference (μ2 - μ1):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Assumed true difference in means between control (μ2) and treatment (μ1). Often assumed to be 0.'); return false;")
                                     ), value=0),
                                     
                                     
                                     
                                     actionButton("ni_means_calc", "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("ni_means_result"),
                                     plotOutput("ni_means_plot", height="280px")
                              )
                            )
                   ),
                   ###────────────────────────────────────
                   ### A.2 Two Proportions (Difference) -----
                   ###────────────────────────────────────
                   tabPanel("Two Proportions (Difference)",
                            fluidRow(
                              column(4,
                                     numericInput("ni_prop_alpha", span(
                                       "Significance Level α:",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('One-sided significance level used for non-inferiority test. Typically set at 0.025.'); return false;")
                                     ), 0.025),
                                     
                                     numericInput("ni_prop_power", span(
                                       "Power (1-β):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Probability of correctly concluding non-inferiority if it truly exists.'); return false;")
                                     ), 0.80),
                                     
                                     numericInput("ni_prop_p1", span(
                                       "p1 (Treatment):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected event proportion in the treatment group.'); return false;")
                                     ), 0.50),
                                     
                                     numericInput("ni_prop_p2", span(
                                       "p2 (Control):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected event proportion in the control group.'); return false;")
                                     ), 0.40),
                                     
                                     numericInput("ni_prop_margin", span(
                                       "NI Margin (Δ):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Non-inferiority margin — maximum acceptable difference (p1 - p2) where treatment is still non-inferior.'); return false;")
                                     ), 0.10),
                                     
                                     numericInput("ni_prop_ratio", span(
                                       "Allocation ratio (n1/n2):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Ratio of sample size between treatment and control groups.'); return false;")
                                     ), 1)
                                     ,
                                     
                                     actionButton("ni_prop_calc", "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("ni_prop_result"),
                                     plotOutput("ni_prop_plot", height="280px")
                              )
                            )
                   ),
                   ###────────────────────────────────────
                   ### A.3 Two Proportions (Ratio) ----
                   ###────────────────────────────────────
                   tabPanel("Two Proportions (Ratio)",
                            fluidRow(
                              column(4,
                                     numericInput("ni_ratio_alpha", span(
                                       "Significance Level α:",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('One-sided significance level, typically 0.025 for non-inferiority hypothesis.'); return false;")
                                     ), 0.025),
                                     
                                     numericInput("ni_ratio_power", span(
                                       "Power (1-β):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Desired statistical power for detecting non-inferiority based on risk ratio.'); return false;")
                                     ), 0.80),
                                     
                                     numericInput("ni_ratio_p1", span(
                                       "p1 (Treatment):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected event rate in the treatment group.'); return false;")
                                     ), 0.50),
                                     
                                     numericInput("ni_ratio_p2", span(
                                       "p2 (Control):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected event rate in the control group.'); return false;")
                                     ), 0.40),
                                     
                                     numericInput("ni_ratio_rmargin", span(
                                       "NI Margin (RR Upper):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Non-inferiority margin for risk ratio — usually an upper bound like 1.25.'); return false;")
                                     ), 1.25),
                                     
                                     numericInput("ni_ratio_alloc", span(
                                       "Allocation ratio (n1/n2):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Ratio of sample sizes between the two groups (treatment/control).'); return false;")
                                     ), 1)
                                     ,
                                     
                                     actionButton("ni_ratio_calc", "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("ni_ratio_result"),
                                     plotOutput("ni_ratio_plot", height="280px")
                              )
                            )
                   )
                 )
        ),
        ##===-===-===-===-===-===-===
        ## 6.2 (B) Equivalence -UI ----
        ##===-===-===-===-===-===-===
        tabPanel("Equivalence",
                 
                 tabsetPanel(
                   ###────────────────────────────────────
                   ### B.1 Two Means (Difference) ----
                   ###────────────────────────────────────
                   tabPanel("Two Means (Difference)",
                            fluidRow(
                              column(4,
                                     numericInput("eq_means_alpha", span(
                                       "Significance Level α (One side):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('One-sided significance level used in equivalence testing. Typically set at 0.05.'); return false;")
                                     ), 0.05),
                                     
                                     numericInput("eq_means_power", span(
                                       "Power (1-β):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Statistical power to correctly conclude equivalence when it exists.'); return false;")
                                     ), 0.80),
                                     
                                     numericInput("eq_means_sigma", span(
                                       "Pooled StdDev (σ):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Estimated pooled standard deviation of the two groups. Used in sample size calculation.'); return false;")
                                     ), 1.5),
                                     
                                     numericInput("eq_means_ratio", span(
                                       "Allocation ratio (n1/n2):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Ratio of sample sizes in group 1 to group 2. Set to 1 for equal allocation.'); return false;")
                                     ), 1),
                                     
                                     numericInput("eq_means_delta", span(
                                       "Equiv. Margin (delta):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Equivalence margin — the maximum allowed difference in means for considering the treatments equivalent.'); return false;")
                                     ), 0.5),
                                     
                                     numericInput("eq_means_margin", span(
                                       "True mean diff (μ2 - μ1):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Assumed true difference in means between the two groups. Often assumed to be 0.'); return false;")
                                     ), 0)
                                     ,
                                     
                                     actionButton("eq_means_calc",   "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("eq_means_result"),
                                     plotOutput("eq_means_plot", height="280px")
                              )
                            )
                   ),
                   ###────────────────────────────────────
                   ### B.2 Two Proportions (Difference) ----
                   ###────────────────────────────────────
                   tabPanel("Two Proportions (Difference)",
                            fluidRow(
                              column(4,
                                     numericInput("eq_prop_alpha", span(
                                       "Significance Level α:",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('One-sided significance level for equivalence testing. Often set at 0.05.'); return false;")
                                     ), 0.05),
                                     
                                     numericInput("eq_prop_power", span(
                                       "Power (1-β):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Statistical power to detect equivalence in two proportions.'); return false;")
                                     ), 0.80),
                                     
                                     numericInput("eq_prop_p1", span(
                                       "p1 (Test):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected event proportion in the test group.'); return false;")
                                     ), 0.50),
                                     
                                     numericInput("eq_prop_p2", span(
                                       "p2 (Control):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected event proportion in the control group.'); return false;")
                                     ), 0.40),
                                     
                                     numericInput("eq_prop_ratio", span(
                                       "Allocation ratio (n1/n2):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Ratio of sample sizes between test and control groups.'); return false;")
                                     ), 1),
                                     
                                     numericInput("eq_prop_delta", span(
                                       "Equiv. Margin (delta):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Maximum acceptable absolute difference in proportions to declare equivalence.'); return false;")
                                     ), 0.1),
                                     
                                     numericInput("eq_prop_margin", span(
                                       "Non-inf/sup margin:",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Optional margin used for additional tests of non-inferiority or superiority, if applicable.'); return false;")
                                     ), 0.15)
                                     ,
                                     
                                     actionButton("eq_prop_calc",   "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("eq_prop_result"),
                                     plotOutput("eq_prop_plot", height="280px")
                              )
                            )
                   ),
                   ###────────────────────────────────────
                   ### B.3 Two Proportions (Ratio) ----
                   ###────────────────────────────────────
                   tabPanel("Two Proportions (Ratio)",
                            fluidRow(
                              column(4,
                                     numericInput("eq_ratio_alpha", span(
                                       "Significance Level α:",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('One-sided alpha used for equivalence testing of risk ratios. Commonly 0.05.'); return false;")
                                     ), 0.05),
                                     
                                     numericInput("eq_ratio_power", span(
                                       "Power (1-β):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Power to correctly detect equivalence based on a risk ratio.'); return false;")
                                     ), 0.80),
                                     
                                     numericInput("eq_ratio_pt", span(
                                       "pt (Test):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected proportion of events in the test group.'); return false;")
                                     ), 0.50),
                                     
                                     numericInput("eq_ratio_pc", span(
                                       "pc (Control):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected proportion of events in the control group.'); return false;")
                                     ), 0.40),
                                     
                                     numericInput("eq_ratio_alloc", span(
                                       "Allocation ratio (nT/nC):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Ratio of sample sizes between test and control groups.'); return false;")
                                     ), 1),
                                     
                                     numericInput("eq_ratio_margin", span(
                                       "Equiv. Margin:",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Equivalence margin in terms of allowable ratio (e.g., 1.25 means equivalence if RR is within [1/1.25, 1.25]).'); return false;")
                                     ), 1.25)
                                     ,
                                     
                                     actionButton("eq_ratio_calc",   "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("eq_ratio_result"),
                                     plotOutput("eq_ratio_plot", height="280px")
                              )
                            )
                   )
                 )
        ),
        ##===-===-===-===-===-===-===
        ## 6.3 (C) Superiority (Equality) -UI ----
        ##===-===-===-===-===-===-===
        tabPanel("Superiority",
                 
                 tabsetPanel(
                   ###────────────────────────────────────
                   ### C.1 Two Means (Difference) ----
                   ###────────────────────────────────────
                   tabPanel("Two Means (Difference)",
                            fluidRow(
                              column(4,
                                     numericInput("sup_means_alpha", span(
                                       "Significance Level α:",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Two-sided significance level for the superiority test. Typically set at 0.05.'); return false;")
                                     ), value = 0.05),
                                     
                                     numericInput("sup_means_power", span(
                                       "Power (1-β):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Desired probability of correctly detecting a true difference in means.'); return false;")
                                     ), value = 0.80),
                                     
                                     numericInput("sup_means_sigma", span(
                                       "Pooled Std Dev (σ):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Estimated pooled standard deviation across groups. Used in sample size calculation.'); return false;")
                                     ), value = 1.5),
                                     
                                     numericInput("sup_means_ratio", span(
                                       "Allocation ratio (n1/n2):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Ratio of sample sizes between treatment group (n1) and control group (n2).'); return false;")
                                     ), value = 1),
                                     
                                     numericInput("sup_means_margin", span(
                                       "Expected Mean Difference (Δ):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Assumed true difference in means under the alternative hypothesis (e.g., μ1 - μ2).'); return false;")
                                     ), value = 0.5)
                                     ,
                                     
                                     actionButton("sup_means_calc",   "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("sup_means_result"),
                                     plotOutput("sup_means_plot", height="280px")
                              )
                            )
                   ),
                   ###────────────────────────────────────
                   ### C.2 Two Proportions (Difference) ----
                   ###────────────────────────────────────
                   tabPanel("Two Proportions (Difference)",
                            fluidRow(
                              column(4,
                                     numericInput("sup_prop_alpha", span(
                                       "Significance Level α:",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Two-sided significance level for the test. Usually set at 0.05.'); return false;")
                                     ), value = 0.05),
                                     
                                     numericInput("sup_prop_power", span(
                                       "Power (1-β):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Probability of detecting a true difference in proportions.'); return false;")
                                     ), value = 0.80),
                                     
                                     numericInput("sup_prop_p1", span(
                                       "p1 (Treatment):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected proportion of success or event in the treatment group.'); return false;")
                                     ), value = 0.5),
                                     
                                     numericInput("sup_prop_p2", span(
                                       "p2 (Control):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected proportion of success or event in the control group.'); return false;")
                                     ), value = 0.4),
                                     
                                     numericInput("sup_prop_ratio", span(
                                       "Allocation ratio (n1/n2):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Sample size ratio between treatment and control groups.'); return false;")
                                     ), value = 1)
                                     ,
                                     
                                     actionButton("sup_prop_calc",   "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("sup_prop_result"),
                                     plotOutput("sup_prop_plot", height="280px")
                              )
                            )
                   ),
                   ###────────────────────────────────────
                   ### C.3 Two Proportions (Ratio) ----
                   ###────────────────────────────────────
                   tabPanel("Two Proportions (Ratio)",
                            fluidRow(
                              column(4,
                                     numericInput("sup_ratio_alpha", span(
                                       "Significance Level α:",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Two-sided alpha level used for risk ratio superiority testing.'); return false;")
                                     ), value = 0.05),
                                     
                                     numericInput("sup_ratio_power", span(
                                       "Power (1-β):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Desired probability to detect a difference in risk ratios.'); return false;")
                                     ), value = 0.80),
                                     
                                     numericInput("sup_ratio_pt", span(
                                       "pt (Treatment):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected event proportion in the treatment group.'); return false;")
                                     ), value = 0.5),
                                     
                                     numericInput("sup_ratio_pc", span(
                                       "pc (Control):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Expected event proportion in the control group.'); return false;")
                                     ), value = 0.4),
                                     
                                     numericInput("sup_ratio_alloc", span(
                                       "Allocation ratio (n1/n2):",
                                       tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                              onclick = "alert('Sample size ratio between treatment and control groups.'); return false;")
                                     ), value = 1)
                                     ,
                                     
                                     actionButton("sup_ratio_calc",  "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("sup_ratio_result"),
                                     plotOutput("sup_ratio_plot", height="280px")
                              )
                            )
                   )
                 )
        ),
                   
                   
                   
      
        
####-###-###-###-###-###-###-###-###
# 7. 生存分析类----
####-###-###-###-###-###-###-###-###
        tabPanel("Survival Analysis",
                 tabsetPanel(
                   
                   
                   ##===-===-===-===-===-===-===
                   ## 7.2 Log-rank Test (TrialSize)----
                   ##===-===-===-===-===-===-===
                   tabPanel("Log-rank Test",
                            h3("Two-Sample Survival Equality (TrialSize)"),
                            fluidRow(
                              column(4,
                                     numericInput("lr_alpha", span("Significance Level (alpha):", 
                                                  tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                         onclick = "alert('Type I error rate for the log-rank test. Typically set at 0.05 for a two-sided test.'); return false;")
                                     ),value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                                     numericInput("lr_beta", span("Beta (1 - Power):",
                                                  tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                         onclick = "alert('Probability of failing to detect a true difference (1 - power).'); return false;")
                                     ), value = 0.2, min = 0.01, max = 0.5, step = 0.01),
                                     numericInput("lr_lam1", span("Hazard Rate for Control Group (λ1):", 
                                                  tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                         onclick = "alert('Event rate (hazard) in the control group. λ = log(2)/median survival time.'); return false;")
                                     ), value = 1, min = 0, step = 0.01),
                                     numericInput("lr_lam2", span("Hazard Rate for Treatment Group (λ2):", 
                                                  tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                         onclick = "alert('Event rate (hazard) in the treatment group.'); return false;")
                                     ),value = 2, min = 0, step = 0.01),
                                     numericInput("lr_k", span("Allocation Ratio (k = n1 / n2):",
                                                  tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                         onclick = "alert('Ratio of sample size in treatment group to control group.'); return false;")
                                     ), value = 1, min = 0.1, max = 10, step = 0.1),
                                     numericInput("lr_ttotal", span("Total Study Duration (T_total):", 
                                                  tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                         onclick = "alert('Total time (in years) that the study will run, including accrual and follow-up.'); return false;")
                                     ), value = 3, min = 1, step = 0.1),
                                     numericInput("lr_taccrual", span("Accrual Duration (T_accrual):", 
                                                  tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                         onclick = "alert('Time period (in years) during which participants are recruited into the study.'); return false;")
                                     ), value = 1, min = 0.5, step = 0.1),
                                     numericInput("lr_gamma", span("Accrual Distribution Parameter (γ):", 
                                                  tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                         onclick = "alert('Rate of random censoring due to dropout or loss to follow-up.'); return false;")
                                     ), value = 0.00001, min = 0, step = 0.00001),
                                     actionButton("lr_calculate", "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("lr_result"),
                                     plotOutput("lr_plot", height = "280px")
                              )
                            )
                   ),
                   
                   ##===-===-===-===-===-===-===
                   ## 7.3 Group Sequential (gsDesign) ----
                   ##===-===-===-===-===-===-===
                   tabPanel("Group Sequential",
                            h3("Group Sequential for Survival Data (gsDesign)"),
                            fluidRow(
                              column(4,
                                     
                                     numericInput("gsd_alpha", span("Significance Level (α)", 
                                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                           onclick = "alert('Overall two-sided significance level for group sequential log-rank test.'); return false;")
                                     ),value = 0.025, min = 0, max = 1),
                                     numericInput("gsd_power", span("Power (1-β)", 
                                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                           onclick = "alert('Probability of detecting a true hazard difference across interim analyses.'); return false;")
                                     ),value = 0.9, min = 0, max = 1),
                                     numericInput("gsd_nlooks", span("Number of Looks (k)", 
                                                                     tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                            onclick = "alert('Number of planned interim analyses, including the final look.'); return false;")
                                     ), value = 3, min = 1),
                                     numericInput("gsd_delta", span("Standardized Effect Size (δ)", 
                                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                           onclick = "alert('Effect size specified as the natural log of the hazard ratio (log(HR)).'); return false;")
                                     ), value = 0.5),
                                     selectInput("gsd_sfu", span("Upper Bound Spending Function", 
                                                                 tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                        onclick = "alert('Choose the method to control Type I error boundary at interim analyses.\\n\\nOF = O\\'Brien-Fleming (conservative)\\nPocock = equal boundaries\\nHSD = Hwang-Shih-DeCani family'); return false;")
                                     ),choices = c("OF", "Pocock", "HSD")),
                                     selectInput("gsd_sfl", span("Lower Bound Spending Function", 
                                                                 tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                        onclick = "alert('Choose method for futility bounds (stopping early for lack of efficacy).\\n\\nOF = O\\'Brien-Fleming\\nPocock = Pocock boundary\\nHSD = Hwang-Shih-DeCani'); return false;")
                                     ),choices = c("OF", "Pocock", "HSD")),
                                     actionButton("gsd_calc", "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("gsd_result"),
                                     plotOutput("gsd_plot", height = "280px")
                              )
                            )
                   ),
                   
                   ##===-===-===-===-===-===-===
                   ## 7.4 Cox Proportional Hazards (TrialSize) ----
                   ##===-===-===-===-===-===-===
                   tabPanel("Cox PH",
                            h3("Cox Proportional Hazards Sample Size (TrialSize)"),
                            fluidRow(
                              column(4,
                                     numericInput("cox_alpha", span("Significance Level (α)", 
                                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                           onclick = "alert('Two-sided alpha for hypothesis testing in Cox regression. Typically 0.05.'); return false;")
                                     ), value = 0.05, min = 0, max = 1),
                                     numericInput("cox_power", span("Power (1-β)", 
                                                                    tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                           onclick = "alert('Probability of detecting a true hazard ratio under Cox model.'); return false;")
                                     ),value = 0.8, min = 0, max = 1),
                                     numericInput("cox_hr", span("Hazard Ratio (HR)", 
                                                                 tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                        onclick = "alert('Assumed true hazard ratio between two groups. HR < 1 indicates benefit.'); return false;")
                                     ),value = 1.5, min = 0),
                                     numericInput("cox_p1", span("Proportion in Treatment Group 1", 
                                                                 tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                        onclick = "alert('Proportion of subjects in the treatment group (group 1).'); return false;")
                                     ), value = 0.5, min = 0, max = 1),
                                     numericInput("cox_d", span("Probability of Observing Event", 
                                                                tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                                                       onclick = "alert('Number of events required to detect the specified hazard ratio with given power and alpha.'); return false;")
                                     ), value = 0.8, min = 0, max = 1),
                                     actionButton("cox_calc", "Calculate")
                              ),
                              column(8,
                                     verbatimTextOutput("cox_result"),
                                     plotOutput("cox_plot", height = "280px")
                                     )
                              )
                            )
                   )
                 ),
####-###-###-###-###-###-###-###-###
# 8. 诊断检验类----
####-###-###-###-###-###-###-###-###
      tabPanel("Diagnostic Test",
         tabsetPanel(
           ##===-===-===-===-===-===-===
           ## 8.1 Diagnostic Test Sample Size----
           ##===-===-===-===-===-===-===
           tabPanel("Diagnostic Test",
                    h3("Diagnostic Test Sample Size (epiR)"),
                    fluidRow(
                      column(4,
                             
                             numericInput("diag_test", span(
                               "Test Performance (Sensitivity or Specificity, 0–1)",
                               tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                      onclick = "alert('Expected value of sensitivity (Se) or specificity (Sp), depending on the parameter being estimated. Value should be between 0 and 1.'); return false;")
                             ), value = 0.9, min = 0, max = 1),
                             
                             selectInput("diag_type", span(
                               "Parameter to Estimate",
                               tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                      onclick = "alert('Choose which test characteristic to estimate sample size for: Sensitivity (Se) or Specificity (Sp).'); return false;")
                             ), choices = c("Sensitivity (Se)" = "se", "Specificity (Sp)" = "sp")),
                             
                             numericInput("diag_py", span(
                               "Prevalence (0–1)",
                               tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                      onclick = "alert('Expected prevalence of disease in the population. Used to determine number of diseased and non-diseased subjects.'); return false;")
                             ), value = 0.1, min = 0, max = 1),
                             
                             numericInput("diag_epsilon", span(
                               "Margin of Error (ε)",
                               tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                      onclick = "alert('Maximum acceptable margin of error for the estimated sensitivity or specificity.'); return false;")
                             ), value = 0.05, min = 0, max = 1),
                             
                             numericInput("diag_alpha", span(
                               "Significance Level (α)",
                               tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                      onclick = "alert('Type I error rate for the confidence interval around Se or Sp. Typically 0.05.'); return false;")
                             ), value = 0.05, min = 0, max = 1),
                             
                             numericInput("diag_power", span(
                               "Target Power",
                               tags$a(tags$i(class = "fa fa-question-circle"), href = "#",
                                      onclick = "alert('Desired probability of achieving the specified margin of error for Se or Sp. Typically 0.8 or 0.9.'); return false;")
                             ), value = 0.8, min = 0, max = 1)
                             ,
                             actionButton("diag_calc", "Calculate")
                      ),
                      column(8,
                             verbatimTextOutput("diag_result"),
                             plotOutput("diag_plot", height = "280px")
                      )
                    )
           )
         )
),
####-###-###-###-###-###-###-###-###
# About & Help 模块----
####-###-###-###-###-###-###-###-###
tabPanel(title ="About&Help",value = "About&Help",
         fluidRow(
           column(
             width = 12,
             h2(i18n$t("about_welcome_h2")),
             p(i18n$t("about_intro_p1")),
             p(i18n$t("about_intro_p2"))
           )
         ),
         hr(),
         fluidRow(
           column(
             width = 12,
             h3("Q&A - ", i18n$t("about_faq_h3")),
             tags$div(class = "blue-stripe-box",
                      tags$b(i18n$t("faq_q1")),
                      p(i18n$t("faq_a1"))
             ),
             tags$div(class = "blue-stripe-box",
                      tags$b(i18n$t("faq_q2")),
                      p(i18n$t("faq_a2"))
             ),
             tags$div(class = "blue-stripe-box",
                      tags$b(i18n$t("faq_q3")),
                      p(i18n$t("faq_a3"))
             )
           )
         ),
         hr(),
         fluidRow(
           column(
             width = 6,
             textInput(
               "contact", 
               label = i18n$t("contact_email_label"), 
               placeholder = "e.g. you@example.com"
               ),
             textAreaInput(
               "comment", 
               label = i18n$t("contact_msg_label"),
               placeholder = "Write your feedback or question here...", 
               height = "150px"
               ),
             actionButton(
               "submit_commentbtn", 
               i18n$t("btn_submit"),
               icon = icon("paper-plane"), 
               class = "btn-primary")
           )
         ),
         br(),
         br(),
         br(),
)


        
      ) # end of tabsetPanel for main categories
    )   # end of mainPanel
  ),     # end of sidebarLayout
tags$footer(
  class = "fixed-footer",  # 与上面 CSS 中的 footer.fixed-footer 对应
  "© 2024-2025 Sample Size Calculation Tool - All rights reserved."
)
)


#--------------- SERVER ---------------
server <- function(input, output, session) {
  
  
  

  
  
  # 当前语言
  lang_r <- reactiveVal("en")        
  
  # 旧按钮被点时 → 改 reactiveVal，并把下拉同步过去
  observeEvent(input$lang_en, {
    lang_r("en")
  })
  observeEvent(input$lang_cn, { lang_r("cn") })
  observeEvent(input$lang_jp, { lang_r("jp") })
  
  # 当右上角下拉改变时 → 改 reactiveVal（防抖动用 req()）
  observeEvent(input$lang_top, {
    req(input$lang_top)
    lang_r(input$lang_top)
  }, ignoreInit = TRUE)
  
  # 统一出口：只要 lang_r() 变 → 更新 i18n、前端文本、以及另一个控件
  observeEvent(lang_r(), {
    cur <- lang_r()
    i18n$set_translation_language(cur)
    update_lang(cur, session)                        # 即时切换所有 i18n$t()
    
    # 同步下拉框选中项（避免按钮→下拉不同步）
    updateSelectInput(session, "lang_top", selected = cur)
  })
  
  
  
  
  
  output$sidebar_instructions <- renderUI({
    # 根据当前选中的tab来判断
    req(input$myInnerTabs) 
    
    if (input$myInnerTabs == "Home") {
      tagList(
        h4(i18n$t("sidebar_home_h4")),
        tags$p(i18n$t("sidebar_home_p1")),
        tags$p(i18n$t("sidebar_home_p2")),
        tags$ul(
          tags$li(i18n$t("tab_ttest")),          
          tags$li(i18n$t("tab_anova")),
          tags$li("Proportion Tests"),
          tags$li("Correlation Tests"),
          tags$li("ANOVA - Two-Factor ANOVA"),
          tags$li("Epidemiological Studies"),
          tags$li("Non-Inferiority"),
          tags$li("Equivalence"),
          tags$li("Superiority"),
          tags$li("Survival Analysis - Log-rank Test"),
          tags$li("Survival Analysis - Group Sequential"),
          tags$li("Survival Analysis - Cox PH"),
          tags$li("Diagnostic Test")
        ),
        tags$p(i18n$t("sidebar_home_p3")),
        tags$p(i18n$t("sidebar_home_p4"))
      )
    } else if (input$myInnerTabs == "Guidance") {
      tagList(
        h4(i18n$t("sidebar_guidance_h4")),
        tags$p(i18n$t("sidebar_guidance_p1")),
        tags$p(i18n$t("sidebar_guidance_p2")),
        tags$p(i18n$t("sidebar_guidance_p3")),
        tags$p(i18n$t("sidebar_guidance_p4"))
      )
    } else if (input$myInnerTabs == "T-test") {
      tagList(
        h4("T-test"),
        tags$p("This section calculates sample size for One-Sample, Paired-Sample, and Two-Independent-Sample t-tests."),
        tags$p("Usage: Fill in values like alpha, power, effect size or standard deviation, and choose alternative hypothesis. Click 'Calculate Sample Size'. Power curves will be displayed."),
        tags$p("Need help? See 'About & Help' tab.")
      )
    } else if (input$myInnerTabs == "ANOVA") {
      tagList(
        h4("ANOVA"),
        tags$p("This page provides tools for calculating sample size for various ANOVA designs: One-Way, Two-Factor, Repeated Measures, and Two-Period Crossover."),
        tags$p("Usage: Input required statistical parameters such as alpha, power, effect size (Cohen’s f), number of groups, etc. Click 'Calculate' to compute."),
        tags$p("For questions, go to 'About & Help'.")
      )
    } else if (input$myInnerTabs == "Proportion Tests") {
      tagList(
        h4("Proportion Tests"),
        tags$p("This section supports sample size calculations for proportion-based designs: single sample, paired, two-independent, and multiple-group comparisons."),
        tags$p("Usage: Enter parameters such as alpha, power, p0, p1, and choose the test direction. Click 'Calculate Sample Size' to proceed."),
        tags$p("Questions? Visit the 'About & Help' section.")
      )
    } else if (input$myInnerTabs == "Correlation Tests") {
      tagList(
        h4("Correlation Tests"),
        tags$p("This module calculates required sample size for correlation studies, including single-correlation and comparison between two correlations."),
        tags$p("Usage: Input correlation coefficients, alpha, power, and test direction. Results and power curves will be shown."),
        tags$p("Need guidance? Refer to 'About & Help'.")
      )
    } else if (input$myInnerTabs == "Epidemiological Studies") {
      tagList(
        h4("Epidemiological Studies"),
        tags$p("This section supports sample size calculations for case-control and cohort studies using odds ratios or relative risks."),
        tags$p("Usage: Provide alpha, power, expected effect sizes (e.g., OR or RR), baseline risks, and group ratios. Click to calculate."),
        tags$p("More details are available in the 'About & Help' section.")
      )
    } else if (input$myInnerTabs == "Non-Inferiority") {
      tagList(
        h4("Non-Inferiority"),
        tags$p("This module supports sample size estimation for non-inferiority trials (mean or proportion differences or ratios)."),
        tags$p("Usage: Set significance level, power, margin of non-inferiority, effect estimates, and allocation ratio. Click 'Calculate'."),
        tags$p("Need help? Go to 'About & Help'.")
      )
    } else if (input$myInnerTabs == "Equivalence") {
      tagList(
        h4("Equivalence"),
        tags$p("Estimate sample size for equivalence studies using mean differences or proportion comparisons."),
        tags$p("Usage: Input alpha, power, equivalence margin, and effect size, then click 'Calculate'."),
        tags$p("Visit 'About & Help' for guidance.")
      )
    } else if (input$myInnerTabs == "Superiority") {
      tagList(
        h4("Superiority"),
        tags$p("Used to calculate sample size for superiority trials (difference or ratio between two means or proportions)."),
        tags$p("Usage: Fill in values for alpha, power, effect size, and group ratio. Then click 'Calculate'."),
        tags$p("Need help? Check the 'About & Help' tab.")
      )
    } else if (input$myInnerTabs == "Survival Analysis") {
      tagList(
        h4("Survival Analysis"),
        tags$p("Provides sample size tools for survival data: Log-rank test, Group Sequential design, Cox model, etc."),
        tags$p("Usage: Input statistical parameters such as hazard rates, allocation ratio, accrual time, and total duration. Click to calculate."),
        tags$p("In the Group Sequential module, the plot displays critical Z-values (normal quantiles) on the y-axis against cumulative sample size or information fraction on the x-axis."),
        tags$p("These boundaries indicate the significance thresholds at each interim look — if your Z-statistic exceeds the upper boundary, you can reject the null hypothesis early."),
        tags$p("Questions? Visit the 'About & Help' tab.")
      )
    } else if (input$myInnerTabs == "Diagnostic Test") {
      tagList(
        h4("Diagnostic Test"),
        tags$p("This section calculates the required sample size for evaluating diagnostic test accuracy based on sensitivity or specificity."),
        tags$p("Usage: Choose whether you're estimating sensitivity or specificity. Enter prevalence, desired precision (margin of error), alpha, and power. Click 'Calculate Sample Size'."),
        tags$p("To learn more or troubleshoot, please go to the 'About & Help' tab.")
      )
    } else if (input$myInnerTabs == "About&Help") {
      tagList(
        h4("About&Help"),
        tags$p("This page provides general guidance, data format instructions, and troubleshooting advice."),
        tags$p("Here you can find frequently asked questions, developer contact info, and usage tips for each module."),
        tags$p("If you experience problems or have suggestions, feel free to reach out to us.")
      )
    }
    else {
      # fallback
      tagList(
        h4("Usage Instructions"),
        tags$p("如果没有匹配，显示默认内容……")
      )
    }
  })
  
  
  
  output$my_slick <- renderSlickR({
    # 图片路径或 URL，可以是本地文件路径或在线图片链接
    image_paths <- c(
      "www/p1.png",
      "www/p2.png" 
    )
    
    # 创建轮播图
    slickR(obj = image_paths, 
           height = 400, 
           width = "100%") # 高度和宽度可以根据需要调整
  })
  
  
  
  # ===== 邮件反馈=====
  observeEvent(input$submit_commentbtn, {
    req(input$contact, input$comment)
    
    # 配置邮件服务器
    smtp <- emayili::server(
      host = "smtp.163.com",  
      port = 25,
      username = "david_yr@163.com",
      password = "QRmEaRWXGY6aSApT"  # 请确保安全，正式部署建议用 .Renviron 隐藏
    )
    
    # 构建邮件内容
    email <- envelope() %>%
      from("david_yr@163.com") %>%
      to("david_yr@163.com") %>%
      cc("smuluopeng@163.com") %>%
      subject(paste("SampleSizeTool FEEDBACK:", input$contact)) %>%
      text(paste("Message:", input$comment, "\nFrom:", input$contact))
    
    # 发送邮件
    smtp(email)
    
    # 弹窗提示
    show_alert(
      title = "Success",
      text = "Thanks for your feedback! We'll get back to you soon.",
      type = "success"
    )
  })
  
  
  
  
  
  
  
  
  
  ####-###-###-###-###-###-###-###-###
  # 引导模块----
  ####-###-###-###-###-###-###-###-###
 
  # Step 1 -> Step 2
  observeEvent(input$step1_next, {
    req(input$step1_design)  # 确保已选择
    # 显示 Step 2 问题，根据 step1_design 生成不同内容
    output$step2_ui <- renderUI({
      # 根据试验设计类型决定问题文本和选项
      if (input$step1_design == "two") {
        # 两组设计 -> 研究目标四选一
        tagList(
          h4(i18n$t("wizard_step2_title")),
          radioButtons("step2_objective", label=NULL,
                       choices = c("检验A组与B组的差异" = "difference",
                                   "证明A组优于B组（优效性）" = "superiority",
                                   "证明A组不劣于B组（非劣效）" = "noninferiority",
                                   "证明A组与B组等效（等效性）" = "equivalence")
          )
        )
      } else if (input$step1_design == "single") {
        # 单组设计 -> 研究目标单一（验证是否达到目标值）
        tagList(
          h4(i18n$t("wizard_step2_title")),
          radioButtons("step2_objective", label=NULL,
                       choices = c("验证结果是否达到预期目标值" = "single_target")
          )
        )
      } else if (input$step1_design == "multi") {
        # 多组设计 -> 研究目标单一（整体差异检验）
        tagList(
          h4(i18n$t("wizard_step2_title")),
          radioButtons("step2_objective", label=NULL,
                       choices = c("比较所有组整体是否存在差异" = "overall")
          )
        )
      } else if (input$step1_design == "special") {
        # 特殊设计 -> 研究目标表示具体设计类型（二选一）
        tagList(
          h4(i18n$t("wizard_step2_title")),
          radioButtons("step2_objective", label=NULL,
                       choices = c("配对设计（同一对象前后测量对比）" = "paired",
                                   "交叉设计（两组交换治疗顺序）" = "crossover")
          )
        )
      }
    })
    shinyjs::show("step2")  # 显示第二步
  })
  
  # Step 2 -> Step 3
  observeEvent(input$step2_next, {
    req(input$step2_objective)  # 确保已回答 Step2
    # 动态生成 Step 3 问题 UI
    output$step3_ui <- renderUI({
      h4(i18n$t("wizard_step3_title"))
      # 根据前面的设计和目标确定数据类型选项
      # 两组设计提供三类选项，其它设计提供二类选项
      if (input$step1_design == "two") {
        radioButtons("step3_data", label=NULL,
                     choices = c("数值型（连续数据，如血压值）" = "numeric",
                                 "二分类（发生/未发生，如治愈）" = "binary",
                                 "生存时间（如生存期，HR）" = "survival")
        )
      } else {
        radioButtons("step3_data", label=NULL,
                     choices = c("数值型（连续数据，如测量值）" = "numeric",
                                 "二分类（事件发生率）" = "binary")
        )
      }
    })
    shinyjs::show("step3")  # 显示第三步
  })
  
  # Step 3 -> Step 4 或 完成
  observeEvent(input$step3_next, {
    req(input$step3_data)
    # 判断是否需要Step4附加问题（仅当两组设计+差异性检验+生存数据时需要）
    if (input$step1_design == "two" && input$step2_objective == "difference" && input$step3_data == "survival") {
      # 生存分析细节问题
      output$step4_ui <- renderUI({
        tagList(
          h4(i18n$t("wizard_step4_title")),
          radioButtons("step4_detail", label=NULL,
                       choices = c("标准生存曲线比较（对数秩检验）" = "logrank",
                                   "成组序贯设计（允许中期分析）" = "groupseq",
                                   "Cox回归模型（考虑协变量）" = "cox")
          )
        )
      })
      shinyjs::show("step4")  # 显示第四步
    } else {
      # 其它情况无附加问题，直接生成结果
      generateRecommendation()
    }
  })
  
  # Step 4 -> 完成 (Finish)
  observeEvent(input$step4_finish, {
    req(input$step4_detail)
    generateRecommendation()
  })
  
  # 根据所有选择生成最终推荐结果
  generateRecommendation <- function() {
    # 确定推荐的方法名称和说明
    # 根据流程图逻辑匹配
    rec_text <- ""
    rec_panel <- NULL  # 将用于跳转对应模块的标识
    # 试验设计:
    design <- input$step1_design
    obj <- input$step2_objective
    dtype <- input$step3_data
    
    if (design == "single") {
      # 单组设计
      if (dtype == "numeric") {
        rec_text <- "建议使用 **单样本 t 检验** 方法计算样本量（比较样本均值与目标值）。"
        rec_panel <- "T-test"  # 对应One-sample t-test模块
      } else if (dtype == "binary") {
        rec_text <- "建议使用 **单样本率检验** 方法计算样本量（比较样本比例与目标值）。"
        rec_panel <- "Proportion Tests"  # 单样本比例检验模块
      }
    } else if (design == "two") {
      # 两组设计
      if (obj == "difference") {
        # 差异性检验
        if (dtype == "numeric") {
          rec_text <- "建议使用 **两独立样本 t 检验** 的样本量公式。提供两组均值差和标准差估计。"
          rec_panel <- "T-test"  # 两样本 t 检验模块
        } else if (dtype == "binary") {
          rec_text <- "建议使用 **两独立样本比例比较检验**（χ²检验）计算样本量。提供对照组发生率和预期差异。"
          rec_panel <- "Proportion Tests"  # 两样本率比较模块
        } else if (dtype == "survival") {
          # 生存数据，根据细分
          if (input$step4_detail == "logrank") {
            rec_text <- "建议使用 **对数秩检验（Log-rank）** 计算生存分析样本量。需要预计的风险比(HR)和随访时间等参数。"
            rec_panel <- "Survival Analysis"  # 生存分析模块
          } else if (input$step4_detail == "groupseq") {
            rec_text <- "建议使用 **生存数据的序贯设计** 方法。允许期中分析，需要设定分析次数和效能边界（如O'Brien-Fleming法）。"
            rec_panel <- "Survival Analysis"  # 序贯生存分析模块（可能与生存分析合并）
          } else if (input$step4_detail == "cox") {
            rec_text <- "建议使用 **Cox 比例风险回归模型** 的样本量估计方法。考虑协变量影响（需估计协变量数量）。"
            rec_panel <- "Survival Analysis"  # Cox 生存分析模块
          }
        }
      } else if (obj == "superiority") {
        # 优效性检验
        if (dtype == "numeric") {
          rec_text <- "建议使用 **双样本均值比较（优效性检验）**，即单侧 t 检验，假设A组均值高于B组。"
          rec_panel <- "Superiority"  # 优效性模块
        } else if (dtype == "binary") {
          rec_text <- "建议使用 **两组比例优效性检验**（单侧χ²检验），假设A组发生率高于B组。"
          rec_panel <- "Superiority"
        } else if (dtype == "survival") {
          rec_text <- "建议使用 **生存分析优效性检验**，例如对数秩检验（单侧），根据预期HR计算所需样本量。"
          rec_panel <- "Superiority"
        }
      } else if (obj == "noninferiority") {
        # 非劣效性检验
        if (dtype == "numeric") {
          rec_text <- "建议使用 **两组均值非劣效性检验** 的公式（单侧 t 检验，需指定非劣效界值）。"
          rec_panel <- "Non-Inferiority"
        } else if (dtype == "binary") {
          rec_text <- "建议使用 **两组比例非劣效检验**（单侧χ²检验，需指定发生率非劣效界限）。"
          rec_panel <- "Non-Inferiority"
        } else if (dtype == "survival") {
          rec_text <- "建议使用 **生存数据非劣效检验** 方法，根据预设的非劣效HR界值计算样本量。"
          rec_panel <- "Non-Inferiority"
        }
      } else if (obj == "equivalence") {
        # 等效性检验
        if (dtype == "numeric") {
          rec_text <- "建议使用 **两组均值等效性检验** 的公式（双侧 t 检验，需要设定等效区间）。"
          rec_panel <- "Equivalence"
        } else if (dtype == "binary") {
          rec_text <- "建议使用 **两组比例等效性检验**（双侧χ²检验，需要设定等效差异范围）。"
          rec_panel <- "Equivalence"
        } else if (dtype == "survival") {
          rec_text <- "建议使用 **生存数据等效性检验** 方法，根据等效风险比区间计算所需样本量。"
          rec_panel <- "Equivalence"
        }
      }
    } else if (design == "multi") {
      # 多组设计
      if (dtype == "numeric") {
        rec_text <- "建议使用 **单因素多组方差分析（one-way ANOVA）** 进行样本量计算，用于检测组间均值是否存在总体差异。"
        rec_panel <- "ANOVA"
      } else if (dtype == "binary") {
        rec_text <- "建议使用 **多组率比较检验**（如卡方检验或Fisher精确检验）来计算样本量，用于比较多组比例差异。"
        rec_panel <- "Proportion Tests"
      }
    } else if (design == "special") {
      # 特殊设计
      if (obj == "paired") {
        # 配对设计
        if (dtype == "numeric") {
          rec_text <- "建议使用 **配对样本 t 检验** 的样本量公式，比较同一对象干预前后的数值变化。"
          rec_panel <- "T-test"  # 配对t检验在T-test模块
        } else if (dtype == "binary") {
          rec_text <- "建议使用 **配对样本率检验**（McNemar 检验）的样本量公式，比较配对前后的事件发生变化。"
          rec_panel <- "Proportion Tests"
        }
      } else if (obj == "crossover") {
        # 交叉设计
        if (dtype == "numeric") {
          rec_text <- "建议使用 **两周期交叉设计均值比较** 的样本量计算方法（考虑受试者作为自身对照的设计）。"
          rec_panel <- "Crossover"  # 若有专门交叉设计模块
        } else if (dtype == "binary") {
          rec_text <- "建议使用 **两周期交叉设计率比较** 的样本量计算方法，适用于交叉试验的二分类结局。"
          rec_panel <- "Crossover"
        }
      }
    }
    # 将结果输出到右侧面板
    output$final_recommendation <- renderUI({
      HTML(rec_text)  # 支持粗体等格式的HTML文本
    })
    # 显示结果面板并隐藏说明面板（如果需要）
    shinyjs::hide("explanation_panel")
    shinyjs::show("result_panel")
    # 保存推荐模块以便跳转
    session$userData$rec_panel <- rec_panel
  }
  
  # 实时更新右侧解释文本
  output$explanation_text <- renderUI({
    # 根据当前已回答的题目，动态列出每步选择的解释说明
    explanations <- list()
    if (!is.null(input$step1_design)) {
      # Step1 解释
      expl <- switch(input$step1_design,
                     "single"   = "单组设计：所有受试者接受同样的处理，无对照组。",
                     "two"      = "两组设计：受试者随机分为两组，分别接受不同处理。",
                     "multi"    = "多组设计：受试者随机分为多组，用于比较多种处理效果。",
                     "special"  = "特殊设计：包括配对设计或交叉设计等非典型随机方案。"
      )
      explanations <- append(explanations, tags$p(strong("试验设计："), expl))
    }
    if (!is.null(input$step2_objective)) {
      # Step2 解释
      obj_expl <- ""
      if (input$step1_design == "two") {
        obj_expl <- switch(input$step2_objective,
                           "difference"     = "差异性检验：检测两组间是否存在显著差异（双侧假设）。",
                           "superiority"    = "优效性：检验实验组效果优于对照组（单侧假设）。",
                           "noninferiority" = "非劣效：检验实验组不比对照组差在可接受范围内。",
                           "equivalence"    = "等效性：检验两组效果无差异，在预设等效区间内。"
        )
      } else if (input$step1_design == "single") {
        obj_expl <- "单组目标：与预期的目标值比较，验证结果是否达标。"
      } else if (input$step1_design == "multi") {
        obj_expl <- "总体差异：检验所有组间是否存在总体显著差异。"
      } else if (input$step1_design == "special") {
        obj_expl <- switch(input$step2_objective,
                           "paired"    = "配对设计：同一受试者接受两次处理/测量，比较前后差异。",
                           "crossover" = "交叉设计：受试者先后接受两种处理，比较不同处理顺序效果。"
        )
      }
      explanations <- append(explanations, tags$p(strong("研究目标："), obj_expl))
    }
    if (!is.null(input$step3_data)) {
      # Step3 解释
      dtype_expl <- switch(input$step3_data,
                           "numeric"  = "连续型数据：测量得到的数值，例如血压、分数等。",
                           "binary"   = "二分类数据：事件是否发生的结果，例如是否患病。",
                           "survival" = "生存数据：关注事件发生所需时间，例如生存期（常用风险比HR表示）。"
      )
      explanations <- append(explanations, tags$p(strong("数据类型："), dtype_expl))
    }
    if (!is.null(input$step4_detail) && input$step1_design == "two" &&
        input$step2_objective == "difference" && input$step3_data == "survival") {
      # Step4 解释（仅当有Step4问题时）
      detail_expl <- switch(input$step4_detail,
                            "logrank"  = "对数秩检验：经典生存曲线比较，不含中期分析或协变量。",
                            "groupseq" = "序贯设计：允许多次中期分析，需调整检验水准以控制总体α。",
                            "cox"      = "Cox回归：考虑协变量影响的生存分析，可提高精度。"
      )
      explanations <- append(explanations, tags$p(strong("生存分析类型："), detail_expl))
    }
    # 将所有解释段落组合输出
    do.call(tagList, explanations)
  })
  
  # 监听“重新开始”按钮，重置引导流程
  observeEvent(input$go_back_restart, {
    # 隐藏所有步骤2-4和结果，显示步骤1
    shinyjs::hide(c("step2", "step3", "step4", "result_panel"))
    shinyjs::show("step1")
    # 重置各输入（可选）
    updateRadioButtons(session, "step1_design", selected = character(0))
    # 重新显示说明面板，清空说明内容
    shinyjs::show("explanation_panel")
    output$explanation_text <- renderUI({ NULL })
  })
  
  # 监听“跳转到模块”按钮，根据 rec_panel 导航到对应计算模块
  observeEvent(input$wizard_go_to_tab, {
    req(session$userData$rec_panel)
    target <- session$userData$rec_panel
    # 更新主界面TabsetPanel到相应模块tab
    if (target == "T-test") {
      updateTabsetPanel(session, "myInnerTabs", selected = "T-test")
    } else if (target == "ANOVA") {
      updateTabsetPanel(session, "myInnerTabs", selected = "ANOVA")
    } else if (target == "Proportion Tests") {
      updateTabsetPanel(session, "myInnerTabs", selected = "Proportion Tests")
    } else if (target == "Survival Analysis") {
      updateTabsetPanel(session, "myInnerTabs", selected = "Survival Analysis")
    } else if (target == "Superiority") {
      updateTabsetPanel(session, "myInnerTabs", selected = "Superiority")
    } else if (target == "Non-Inferiority") {
      updateTabsetPanel(session, "myInnerTabs", selected = "Non-Inferiority")
    } else if (target == "Equivalence") {
      updateTabsetPanel(session, "myInnerTabs", selected = "Equivalence")
    } else if (target == "Crossover") {
      updateTabsetPanel(session, "myInnerTabs", selected = "Epidemiological Studies")  # 假设交叉设计在该模块，视实际情况调整
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####-###-###-###-###-###-###-###-###
  # (1) T 检验类----
  ####-###-###-###-###-###-###-###-###
  
  ##===-===-===-===-===-===-===
  ## (1.1) One-sample t-test----
  ##===-===-===-===-===-===-===
  
  observeEvent(input$t1_calc, {
    alpha <- input$t1_alpha    # 显著性水平
    power_req <- input$t1_power    # 检验效能 (1 - β)
    delta <- input$t1_delta    # 均值差
    sd_est <- input$t1_sd       # 标准差(估计)
    alt <- input$t1_alternative
    
    # 1. 计算并显示结果
    d_value <- delta / sd_est   # Cohen's d
    res <- pwr::pwr.t.test(
      d           = d_value,
      sig.level   = alpha,
      power       = power_req,
      type        = "one.sample",
      alternative = alt
    )
    
    
    output$t1_result <- renderPrint({
      cat(
        "---------------------------------\n",
        " One-sample t-test power calculation\n",
        "---------------------------------\n",
        sprintf(" Sample Size (n)        = %.2f\n", res$n),
        sprintf(" Effect Size (d)        = %.3f\n", res$d),
        sprintf(" Significance Level     = %.3f\n", res$sig.level),
        sprintf(" Power                  = %.3f\n", res$power),
        sprintf(" Alternative Hypothesis = %s\n",   res$alternative),
        sep=""
      )
    })
    
    
    
    # 2. 绘制两条功效曲线（并排：样本量 vs. 功效、效应量 vs. 功效），使用 patchwork 方式
    
    ## 2.1 样本量 vs. 功效
    n_seq <- seq(5, 200, by = 5)
    power_n <- sapply(n_seq, function(n_i) {
      tmp <- pwr::pwr.t.test(
        d           = d_value,
        n           = n_i,
        sig.level   = alpha,
        type        = "one.sample",
        alternative = alt
      )
      tmp$power
    })
    df_n <- data.frame(n = n_seq, power = power_n)
    
    p1 <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Sample Size (n)",
        y = "Power",
        title = "Power vs. Sample Size"
      ) +
      theme_minimal(base_size = 14)
    
    ## 2.2 效应量 vs. 功效
    # 固定一个 n_default
    n_default <- 50
    d_seq <- seq(0.1, 1.5, by = 0.1)
    power_d <- sapply(d_seq, function(d_i) {
      tmp <- pwr::pwr.t.test(
        d           = d_i,
        n           = n_default,
        sig.level   = alpha,
        type        = "one.sample",
        alternative = alt
      )
      tmp$power
    })
    df_d <- data.frame(d = d_seq, power = power_d)
    
    p2 <- ggplot(df_d, aes(x = d, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Effect Size (Cohen's d)",
        y = "Power",
        title = paste("Power vs. Effect Size\n(n=", n_default, ")", sep="")
      ) +
      theme_minimal(base_size = 14)
    
    ## 2.3 并排输出
    output$t1_power_curve <- renderPlot({
      p1 + p2  # patchwork 语法，将两张图并排组合
    })
  })
  
  ##===-===-===-===-===-===-===
  ## (1.2) Paired t-test----
  ##===-===-===-===-===-===-===
  observeEvent(input$paired_calc, {
    alpha <- input$paired_alpha
    power <- input$paired_power
    d_value <- input$paired_d
    alt <- input$paired_alternative
    
    # 1. 计算并显示结果
    res <- pwr::pwr.t.test(
      d           = d_value,
      sig.level   = alpha,
      power       = power,
      type        = "paired",
      alternative = alt
    )
    
    output$paired_result <- renderPrint({
      cat(
        "---------------------------------\n",
        " Paired t-test power calculation\n",
        "---------------------------------\n",
        sprintf(" Sample Size (n)        = %.2f\n", res$n),
        sprintf(" Effect Size (d)        = %.3f\n", res$d),
        sprintf(" Significance Level     = %.3f\n", res$sig.level),
        sprintf(" Power                  = %.3f\n", res$power),
        sprintf(" Alternative Hypothesis = %s\n",   res$alternative),
        sep = ""
      )
    })
    
    
    # 2. 绘制两条功效曲线（分别是样本量 vs. 功效、效应量 vs. 功效），再用 patchwork 并排拼贴
    
    ## 2.1 样本量 vs. 功效
    n_seq <- seq(5, 200, by = 5)
    power_n <- sapply(n_seq, function(n_i) {
      tmp <- pwr::pwr.t.test(
        d           = d_value,
        n           = n_i,
        sig.level   = alpha,
        type        = "paired",
        alternative = alt
      )
      tmp$power
    })
    df_n <- data.frame(n = n_seq, power = power_n)
    
    p1 <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Sample Size (n)",
        y = "Power",
        title = "Power vs. Sample Size"
      ) +
      theme_minimal(base_size = 14)
    
    ## 2.2 效应量 vs. 功效
    n_default <- 50
    d_seq <- seq(0.1, 1.5, by = 0.1)
    power_d <- sapply(d_seq, function(d_i) {
      tmp <- pwr::pwr.t.test(
        d           = d_i,
        n           = n_default,
        sig.level   = alpha,
        type        = "paired",
        alternative = alt
      )
      tmp$power
    })
    df_d <- data.frame(d = d_seq, power = power_d)
    
    p2 <- ggplot(df_d, aes(x = d, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Effect Size (Cohen's d)",
        y = "Power",
        title = paste("Power vs. Effect Size\n(n=", n_default, ")", sep="")
      ) +
      theme_minimal(base_size = 14)
    
    ## 2.3 并排输出
    output$paired_power_curves <- renderPlot({
      p1 + p2  # patchwork 语法，表示将两张图并排
    })
  })
  
  
  ##===-===-===-===-===-===-===
  ## (1.3) Two-sample t-test----
  ##===-===-===-===-===-===-===
  observeEvent(input$ind_two_calc, {
    alpha <- input$ind_two_alpha
    power <- input$ind_two_power
    d_value <- input$ind_two_d
    alt <- input$ind_two_alternative
    
    # 1. 计算并显示结果
    res <- pwr::pwr.t.test(
      d           = d_value,
      sig.level   = alpha,
      power       = power,
      type        = "two.sample",
      alternative = alt
    )
    
    
    output$ind_two_result <- renderPrint({
      cat(
        "---------------------------------\n",
        " Two-sample t-test power calculation\n",
        "---------------------------------\n",
        sprintf(" Sample Size (per group) = %.2f\n", res$n),
        sprintf(" Effect Size (d)         = %.3f\n", res$d),
        sprintf(" Significance Level      = %.3f\n", res$sig.level),
        sprintf(" Power                   = %.3f\n", res$power),
        sprintf(" Alternative Hypothesis  = %s\n",   res$alternative),
        sep = ""
      )
    })
    
   
    
    # 2. 绘制两条功效曲线，并排放置
    
    ## 2.1 样本量 vs. 功效
    n_seq <- seq(5, 200, by = 5)
    power_n <- sapply(n_seq, function(n_i) {
      tmp <- pwr::pwr.t.test(
        d           = d_value,
        n           = n_i,
        sig.level   = alpha,
        type        = "two.sample",
        alternative = alt
      )
      tmp$power
    })
    df_n <- data.frame(n = n_seq, power = power_n)
    
    p1 <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Sample Size (n)",
        y = "Power",
        title = "Power vs. Sample Size"
      ) +
      theme_minimal(base_size = 14)
    
    ## 2.2 效应量 vs. 功效
    n_default <- 50
    d_seq <- seq(0.1, 1.5, by = 0.1)
    power_d <- sapply(d_seq, function(d_i) {
      tmp <- pwr::pwr.t.test(
        d           = d_i,
        n           = n_default,
        sig.level   = alpha,
        type        = "two.sample",
        alternative = alt
      )
      tmp$power
    })
    df_d <- data.frame(d = d_seq, power = power_d)
    
    p2 <- ggplot(df_d, aes(x = d, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Effect Size (Cohen's d)",
        y = "Power",
        title = paste("Power vs. Effect Size\n(n=", n_default, ")", sep="")
      ) +
      theme_minimal(base_size = 14)
    
    ## 2.3 并排输出
    output$ind_two_power_curves <- renderPlot({
      p1 + p2  # patchwork 并排组合
    })
  })
  
  ####-###-###-###-###-###-###-###-###
  #(2) 方差分析部分----
  ####-###-###-###-###-###-###-###-###
  
  
  
  ##===-===-===-===-===-===-===
  ## (2.1) 二阶段交叉设计样本量估算----
  ##===-===-===-===-===-===-===
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$crossover_calc, {
    # 1. 获取用户输入
    alpha <- input$crossover_alpha      # 显著性水平
    power_req <- input$crossover_power  # 目标检验功效
    CV_value <- input$crossover_cv      # 系数变异度 Coefficient of Variation
    ratio0 <- input$crossover_ratio     # 真实比值 (θ0)
    
    # 2. 计算结果 (以 2x2 crossover 为例)
    library(PowerTOST)
    res <- sampleN.TOST(
      CV          = CV_value,
      theta0      = ratio0,
      targetpower = power_req,
      alpha       = alpha,
      design      = "2x2"
    )
    
    # 3. 显示结果
    output$crossover_result <- renderPrint({
      cat(
        "---------------------------------------------------\n",
        " Two-Period (2x2) Crossover Design Sample Size Calc\n",
        "---------------------------------------------------\n",
        sprintf(" Design              = %s\n",    res[["Design"]]),
        sprintf(" alpha               = %.2f\n",   res[["alpha"]]),
        sprintf(" CV                  = %.3f\n",   res[["CV"]]),
        sprintf(" theta0 (True Ratio) = %.2f\n",   res[["theta0"]]),
        sprintf(" theta1 (Lower BE)   = %.2f\n",   res[["theta1"]]),
        sprintf(" theta2 (Upper BE)   = %.2f\n",   res[["theta2"]]),
        sprintf(" Sample size (total) = %d\n",     res[["Sample size"]]),
        sprintf(" Achieved power      = %.6f\n",   res[["Achieved power"]]),
        sprintf(" Target power        = %.6f\n",   res[["Target power"]]),
        sep = ""
      )
    })
    
    # 将计算出来的样本量（总受试者数）保存下来，供作图时使用
    n_calc <- as.numeric(res[["Sample size"]])
    
    # 4. 作图：示例展示 “样本量 vs. Power” 以及 “比值 (theta0) vs. Power”
    ## 4.1 样本量 vs. Power
    n_seq <- seq(8, 80, by = 4)
    power_n <- sapply(n_seq, function(n_i) {
      power.TOST(
        CV     = CV_value,
        theta0 = ratio0,
        n      = n_i,
        alpha  = alpha,
        design = "2x2"
      )
    })
    df_n <- data.frame(n = n_seq, power = power_n)
    
    p1 <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Sample Size (Total Subjects)",
        y = "Power",
        title = "Power vs. Sample Size"
      ) +
      theme_minimal(base_size = 14)
    
    ## 4.2 比值 vs. Power (动态使用计算出来的 n_calc)
    theta0_seq <- seq(0.8, 1.2, by = 0.05)
    power_theta0 <- sapply(theta0_seq, function(ratio_i) {
      power.TOST(
        CV     = CV_value,
        theta0 = ratio_i,
        n      = n_calc,     # <-- 与上面计算出的 n_calc 对应
        alpha  = alpha,
        design = "2x2"
      )
    })
    df_ratio <- data.frame(ratio = theta0_seq, power = power_theta0)
    
    p2 <- ggplot(df_ratio, aes(x = ratio, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "θ0 (True Ratio)",
        y = "Power",
        title = paste("Power vs. Ratio (n=", n_calc, ")", sep="")
      ) +
      theme_minimal(base_size = 14)
    
    # 5. 并排输出
    output$crossover_power_curves <- renderPlot({
      p1 + p2
    })
  })
  
  
  
  
  
  ##===-===-===-===-===-===-===
  ## (2.2) One-Way ANOVA----
  ##===-===-===-===-===-===-===

  observeEvent(input$anova1_calc, {
    # 1. 获取用户输入
    alpha <- input$anova1_alpha
    power_req <- input$anova1_power
    f_value <- input$anova1_f          # Cohen's f
    k_groups <- input$anova1_k         # 分组数量
    
    # 2. 计算并显示结果: pwr.anova.test(k, f, sig.level, power)
    res <- pwr.anova.test(
      k         = k_groups,
      f         = f_value,
      sig.level = alpha,
      power     = power_req
    )
    
    # 2.1 将计算结果中的样本量(每组)提取出来，供后续绘图用
    #     注意res$n 为每组样本量；总样本量 = k_groups * res$n
    n_calc <- ceiling(res$n)  # 可加ceil()取整，或保持原状
    
    # 3. 自定义输出格式，使之与前面模块风格统一
    output$anova1_result <- renderPrint({
      cat(
        "-----------------------------------------\n",
        " Balanced One-Way ANOVA Power Calculation\n",
        "-----------------------------------------\n",
        sprintf(" Number of groups (k)      = %d\n",    res$k),
        sprintf(" Sample size per group (n) = %.2f\n",  res$n),
        sprintf(" Cohen's f (effect size)   = %.2f\n",  res$f),
        sprintf(" Significance level        = %.2f\n",  res$sig.level),
        sprintf(" Achieved power            = %.3f\n",  res$power),
        "\nNOTE: n is number in each group.\n",
        sep = ""
      )
    })
    
    # 4. 作图
    ## 4.1 样本量 vs. Power
    #     n_seq 是“每组样本量”，一共 k_groups 组
    n_seq <- seq(5, 100, by = 5)
    power_n <- sapply(n_seq, function(n_each) {
      tmp <- pwr.anova.test(
        k         = k_groups,
        f         = f_value,
        sig.level = alpha,
        n         = n_each
      )
      tmp$power
    })
    df_n <- data.frame(n_each = n_seq, power = power_n)
    
    p1 <- ggplot(df_n, aes(x = n_each, y = power)) +
      geom_line(color = "blue", size = 1) +
      labs(
        x = "Sample Size per Group",
        y = "Power",
        title = paste("Power vs. Sample Size (k =", k_groups, "groups)")
      ) +
      ylim(0, 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      theme_minimal(base_size = 14)
    
    ## 4.2 效应量 f vs. Power (动态使用计算得出的 n_calc)
    f_seq <- seq(0.1, 0.8, by = 0.1)
    power_f <- sapply(f_seq, function(f_i) {
      tmp <- pwr.anova.test(
        k         = k_groups,
        f         = f_i,
        sig.level = alpha,
        n         = n_calc  # <-- 使用计算出来的 n_calc 统一绘图
      )
      tmp$power
    })
    df_f <- data.frame(f = f_seq, power = power_f)
    
    p2 <- ggplot(df_f, aes(x = f, y = power)) +
      geom_line(color = "blue", size = 1) +
      labs(
        x = "Effect Size (Cohen's f)",
        y = "Power",
        title = paste("Power vs. Effect Size\n(n=", n_calc, " per group)")
      ) +
      ylim(0, 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      theme_minimal(base_size = 14)
    
    # 5. 并排输出
    output$anova1_power_curves <- renderPlot({
      p1 + p2
    })
  })
  
  
  
  
  
  
  

  ##===-===-===-===-===-===-===
  ## (2.3) Two-Factor ANOVA----
  ##===-===-===-===-===-===-===
  
  
  observeEvent(input$calc_power, {
    
    # 1) 读取用户输入
    a             <- input$a
    b             <- input$b
    alpha         <- input$alpha
    fA            <- input$fA
    fB            <- input$fB
    target_power  <- input$target_power
    
    # 2) 枚举一批可能的 n 值 (示例: 2~200)
    n_candidates <- 2:200
    
    # 准备记录
    powerA_vec  <- numeric(length(n_candidates))
    powerB_vec  <- numeric(length(n_candidates))
    power_min_vec <- numeric(length(n_candidates))
    
    # 3) 循环计算 pwr.2way
    for(i in seq_along(n_candidates)) {
      n_i <- n_candidates[i]
      tmp <- pwr.2way(
        a       = a,
        b       = b,
        alpha   = alpha,
        size.A  = n_i,
        size.B  = n_i,
        f.A     = fA,
        f.B     = fB
      )
      powerA_vec[i]     <- tmp$power.A
      powerB_vec[i]     <- tmp$power.B
      power_min_vec[i]  <- tmp$power  # pwr.2way 中，这通常是 min(powerA, powerB)
    }
    
    # 4) 找到满足 "Factor A 与 Factor B 功效均 >= target_power" 的最小 n
    idx_meet <- which(powerA_vec >= target_power & powerB_vec >= target_power)
    
    out_txt <- ""
    if (length(idx_meet) == 0) {
      out_txt <- paste0(
        "在 n=2~200 范围内，没有发现能让两主效应功效都 ≥ ", 
        target_power, " 的 n。"
      )
    } else {
      best_n <- n_candidates[min(idx_meet)]
      out_txt <- paste0(
        "若要求 Factor A、Factor B 功效均 ≥ ", target_power, 
        "，则需要每组样本量 n ≥ ", best_n, "。"
      )
    }
    
    # 5) 输出汇总文字
    output$anova2_result <- renderPrint({
      cat(
        "------------------------------------------\n",
        " Two-Factor ANOVA Sample Size Calculation\n",
        "------------------------------------------\n",
        sprintf(" Factor A Levels (a):         %d\n", a),
        sprintf(" Factor B Levels (b):         %d\n", b),
        sprintf(" Significance Level (alpha):  %.3f\n", alpha),
        sprintf(" Effect Size for Factor A:    %.3f\n", fA),
        sprintf(" Effect Size for Factor B:    %.3f\n", fB),
        sprintf(" Target Power:                %.2f\n", target_power),
        "\n"
      )
      
      if (length(idx_meet) == 0) {
        cat("No sample size found within n = 2 to 200 that achieves\nthe target power for both Factor A and Factor B.\n")
      } else {
        best_n <- n_candidates[min(idx_meet)]
        cat(sprintf("Recommended Sample Size per Cell: %d\n", best_n))
        cat("(Both factors reach or exceed the target power.)\n")
      }
      
      cat("\nNOTE: n refers to the sample size per cell (i.e., per combination of Factor A × Factor B).\n")
    })
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##===-===-===-===-===-===-===
  ## (2.4) Repeated-Measures ANOVA ----
  ##===-===-===-===-===-===-===
  
  observeEvent(input$calcArOneBtn, {
    
    # 读入各参数
    rhoVal       <- input$arOneRho
    sdVal        <- input$arOneSD
    tVal         <- input$arOneT
    deltaVal     <- input$arOneDelta
    alphaVal     <- input$arOneAlpha
    desiredPower <- input$arOnePower
    
    # 调用 power.mmrm.ar1()
    # 这里示例只做两组(lambda=1)，如果需要多组设计请自行修改或使用其他函数
    res <- power.mmrm.ar1(
      N           = NULL,           # 要函数求解“每组”所需样本量
      rho         = rhoVal,
      ra          = rep(1, tVal),   # 假设在所有测量时间点都没有脱落
      sigmaa      = sdVal,
      rb          = NULL,
      sigmab      = NULL,          # 若为 NULL，则与 sigmaa 相同
      lambda      = 1,             # 1:1 两组分配
      times       = 1:tVal,
      delta       = deltaVal,
      sig.level   = alphaVal,
      power       = desiredPower,
      alternative = "two.sided"
    )
    
    # 提取每组所需样本量
    nPerGroup <- res$n1   # 对于1:1分组, n1 = n2
    
    # 以指定风格打印结果
    # 这里演示类似 “Balanced One-Way ANOVA Power Calculation”的样式
    resultText <- paste0(
      "-----------------------------------------\n",
      " Repeated Measures (AR(1)) Power Calculation\n",
      "-----------------------------------------\n",
      sprintf(" Number of groups (k)        = %d\n", 2),
      sprintf(" Sample size per group (n)   = %.2f\n", nPerGroup),
      sprintf(" AR(1) correlation (rho)     = %.2f\n", rhoVal),
      sprintf(" Mean difference (delta)     = %.2f\n", deltaVal),
      sprintf(" Significance level (alpha)  = %.2f\n", alphaVal),
      sprintf(" Achieved power              = %.3f\n", desiredPower),
      "\nNOTE: n is number in each group."
    )
    
    output$arOneResult <- renderText({
      resultText
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####-###-###-###-###-###-###-###-###
  # (3) 比例检验----
  ####-###-###-###-###-###-###-###-###
  
  ##===-===-===-===-===-===-===
  ## (3.1) One-Sample Proportion Test----
  ##===-===-===-===-===-===-===
  

  
  
  
  
  
  
  observeEvent(input$prop_one_calc, {
    
    # 0) 读取输入值
    alpha       <- input$prop_one_alpha
    power_req   <- input$prop_one_power
    p0          <- input$prop_one_p0
    p1          <- input$prop_one_p1
    alt         <- input$prop_one_alternative
    
    # 1) 计算效应量 Cohen's h
    #    h = 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p0))
    h_value <- pwr::ES.h(p1 = p1, p2 = p0)
    
    # 2) 使用 pwr.2p.test() 做近似计算
    #    注意：这会返回“每组所需样本量” res$n
    #    对“一样本比例”来说，可视为：假想的对照组(p0) + 实际组(p1)
    res <- pwr::pwr.2p.test(
      h           = h_value,
      sig.level   = alpha,
      power       = power_req,
      alternative = alt
    )
    
    # 2.1 提取每组样本量 (浮点数)，若需要可用 round() 或 ceiling()
    n_calc <- ceiling(res$n)
    
    # 3) 在界面上以自定义排版显示结果
    output$prop_one_result <- renderPrint({
      cat(
        "-----------------------------------------------\n",
        " One-Sample Proportion Test (approx via 2p.test)\n",
        "-----------------------------------------------\n",
        sprintf(" Cohen's h (effect size) = %.6f\n", res$h),
        sprintf(" Sample size per group   = %.2f\n", res$n),
        sprintf(" Significance level      = %.3f\n", res$sig.level),
        sprintf(" Achieved power          = %.3f\n", res$power),
        sprintf(" Alternative hypothesis  = %s\n",   res$alternative),
        "\nNOTE: 'n' is interpreted as sample size per group in a 2-sample approximation.\n",
        sep = ""
      )
    })
    
    # 4) 绘制功效曲线
    
    # (a) 样本量 vs. Power
    #     动态范围: 从 (n_calc - 200) 到 (n_calc + 200), 步长 = 5
    #     防止 n_calc < 200 时出现负数或 0 的情况，故做一下下限保护
    n_min <- max(2, n_calc - 200)  # 下限不能小于2
    n_max <- n_calc + 200
    n_seq <- seq(n_min, n_max, by = 5)
    
    power_n <- sapply(n_seq, function(n_i) {
      tmp <- pwr::pwr.2p.test(
        h           = h_value,
        n           = n_i,
        sig.level   = alpha,
        alternative = alt
      )
      tmp$power
    })
    
    df_n <- data.frame(n = n_seq, power = power_n)
    
    p1 <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      ylim(0, 1) +
      labs(
        x = "Sample Size (per group, approx.)",
        y = "Power",
        title = paste0("Power vs Sample Size\n(h = ", round(res$h, 3), ")")
      ) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      theme_minimal(base_size = 14)
    
    # (b) 备择比例 p1 vs. Power
    #     将原先固定的 n_default=50 改为实际计算得到的 n_calc
    p1_seq <- seq(0, 1, by = 0.05)
    
    power_p1 <- sapply(p1_seq, function(pp) {
      h_val <- pwr::ES.h(pp, p0)
      tmp <- pwr::pwr.2p.test(
        h           = h_val,
        n           = n_calc,
        sig.level   = alpha,
        alternative = alt
      )
      tmp$power
    })
    
    df_p <- data.frame(p1_val = p1_seq, power = power_p1)
    
    p2 <- ggplot(df_p, aes(x = p1_val, y = power)) +
      geom_line(color = "blue", size = 1) +
      ylim(0, 1) +
      labs(
        x = "Alternative Proportion (p1)",
        y = "Power",
        title = paste0("Power vs p1\n(n = ", n_calc, " per group)")
      ) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      theme_minimal(base_size = 14)
    
    # 5) 并排输出
    output$prop_one_power_curve <- renderPlot({
      p1 + p2
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##===-===-===-===-===-===-===
  ## (3.2) Paired Proportion Comparison----
  ##===-===-===-===-===-===-===
  observeEvent(input$prop_paired_calc, {
    alpha       <- input$prop_paired_alpha       # 显著性水平
    power_req   <- input$prop_paired_power       # 目标功效
    p1_val      <- input$prop_paired_p1
    p2_val      <- input$prop_paired_p2
    
    # 简单假设：p10 = p1*(1 - p2), p01 = (1 - p1)*p2
    p10 <- p1_val * (1 - p2_val)
    p01 <- (1 - p1_val) * p2_val
    
    psai <- if (p10 == 0) {
      if (p01 == 0) 1 else Inf
    } else {
      p01 / p10
    }
    paid <- p10 + p01
    
    beta_val <- 1 - power_req
    
    # 调用TrialSize
    library(TrialSize)
    N_est <- McNemar.Test(alpha, beta_val, psai, paid)
    
    output$prop_paired_result <- renderPrint({
      cat(
        "-----------------------------------------------\n",
        " McNemar Test for Paired Proportions (TrialSize)\n",
        "-----------------------------------------------\n",
        sprintf(" alpha (two-sided) = %.3f\n", alpha),
        sprintf(" power             = %.3f\n", power_req),
        sprintf(" p10               = %.4f\n", p10),
        sprintf(" p01               = %.4f\n", p01),
        sprintf(" psai = p01/p10    = %.4f\n", psai),
        sprintf(" paid = p01+p10    = %.4f\n", paid),
        "\n",
        sprintf(" => Required N pairs = %.2f\n", N_est)
      )
    })
  })
  
  
  
  ##===-===-===-===-===-===-===
  ## (3.3) Two-Independent Proportions Comparison----
  ##===-===-===-===-===-===-===
  
  observeEvent(input$prop_two_calc, {
    
    # 1) 读取输入
    alpha     <- input$prop_two_alpha
    power_req <- input$prop_two_power
    p1_val    <- input$prop_two_p1
    p2_val    <- input$prop_two_p2
    alt       <- input$prop_two_alternative
    
    # 2) 计算 Cohen's h
    h_value <- pwr::ES.h(p1_val, p2_val)
    
    # 3) 用 pwr.2p.test() 计算并提取结果
    res <- pwr::pwr.2p.test(
      h           = h_value,
      sig.level   = alpha,
      power       = power_req,
      alternative = alt
    )
    
    # 3.1 提取每组所需样本量 (浮点数)，可以取 ceiling 或 round
    n_calc <- ceiling(res$n)
    
    # 4) 在界面上自定义格式输出
    output$prop_two_result <- renderPrint({
      cat(
        "---------------------------------------------------------\n",
        " Two-Independent Proportions Test (via pwr.2p.test)\n",
        "---------------------------------------------------------\n",
        " Difference of proportion power calculation \n",
        " for binomial distribution (arcsine transformation)\n\n",
        sprintf(" Cohen's h (effect size) = %.6f\n", res$h),
        sprintf(" Sample size per group   = %.2f\n", res$n),
        sprintf(" Significance level      = %.3f\n", res$sig.level),
        sprintf(" Achieved power          = %.3f\n", res$power),
        sprintf(" Alternative hypothesis  = %s\n",   res$alternative),
        "\nNOTE: 'n' is the sample size per group.\n",
        sep = ""
      )
    })
    
    # 5) 绘图
    # (a) 样本量 vs Power
    #    先扫描样本量区间, 例如 [n_calc-200, n_calc+200], 步长5(或10)
    n_min <- max(2, n_calc - 200)
    n_max <- n_calc + 200
    n_seq <- seq(n_min, n_max, by = 5)
    
    power_n <- sapply(n_seq, function(n_i) {
      tmp <- pwr::pwr.2p.test(
        h           = h_value,
        n           = n_i,
        sig.level   = alpha,
        alternative = alt
      )
      tmp$power
    })
    
    df_n <- data.frame(n = n_seq, power = power_n)
    
    p1 <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      ylim(0, 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      labs(
        x = "Sample Size (per group)",
        y = "Power",
        title = paste0("Power vs Sample Size\n(h = ", round(res$h, 3), ")")
      ) +
      theme_minimal(base_size = 14)
    
    # (b) p2 vs Power
    #     将原先 n_default=50 改为 n_calc
    p2_seq <- seq(0, 1, by = 0.05)
    power_p2 <- sapply(p2_seq, function(pp) {
      h_tmp <- pwr::ES.h(p1_val, pp)
      tmp <- pwr::pwr.2p.test(
        h           = h_tmp,
        n           = n_calc,   # 动态使用实际的 n_calc
        sig.level   = alpha,
        alternative = alt
      )
      tmp$power
    })
    
    df_p2 <- data.frame(p2 = p2_seq, power = power_p2)
    
    p2_plot <- ggplot(df_p2, aes(x = p2, y = power)) +
      geom_line(color = "blue", size = 1) +
      ylim(0, 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      labs(
        x = "Proportion 2 (p2)",
        y = "Power",
        title = paste("Power vs p2\n(n =", n_calc, " per group)")
      ) +
      theme_minimal(base_size = 14)
    
    # 6) 并排显示两张图
    output$prop_two_power_curve <- renderPlot({
      p1 + p2_plot
    })
  })
  
  
  
  ##===-===-===-===-===-===-===
  ## (3.4) Multiple Proportions (Completely Randomized)----
  ##===-===-===-===-===-===-===
  observeEvent(input$prop_multi_calc, {
    
    alpha     <- input$prop_multi_alpha
    power_req <- input$prop_multi_power
    
    # 获取用户输入的各组名义比例 (示例3组)
    grp_prop <- c(
      input$prop_multi_grp1,
      input$prop_multi_grp2,
      input$prop_multi_grp3
    )
    # 确保总和=1（或接近1）
    sum_p <- sum(grp_prop)
    if(abs(sum_p - 1) > 1e-6) {
      showNotification("Warning: The group proportions do not sum to 1!", type="warning")
    }
    # 做归一化（如果差异很小）
    grp_prop <- grp_prop / sum_p
    
    k       <- length(grp_prop)
    df_val  <- k - 1
    
    # 1) 计算 Cohen’s w
    #    零假设：各组比例相等 => p0[i] = 1/k
    p0 <- rep(1/k, k)
    w_value <- sqrt( sum( (grp_prop - p0)^2 / p0 ) )
    
    # 2) pwr.chisq.test
    res <- pwr::pwr.chisq.test(
      w         = w_value,
      df        = df_val,
      sig.level = alpha,
      power     = power_req
    )
    
    # 2.1 如果需要的话，取 N_calc = ceiling(res$N)
    N_calc <- ceiling(res$N)
    
    # 3) 统一输出风格
    output$prop_multi_result <- renderPrint({
      cat(
        "-----------------------------------------------------------\n",
        " Multiple Proportions Test (Completely Randomized)\n",
        "-----------------------------------------------------------\n",
        sprintf(" Group proportions: %s\n", 
                paste(round(grp_prop, 3), collapse=" ")),
        sprintf(" Cohen's w          = %.6f\n", res$w),
        sprintf(" N (total)          = %.2f\n", res$N),
        sprintf(" df                 = %d\n",   res$df),
        sprintf(" sig.level          = %.2f\n", res$sig.level),
        sprintf(" power              = %.3f\n", res$power),
        "\nNOTE: N is the total sample size.\n",
        sep = ""
      )
    })
    
    # 4) 绘制仅一幅图：样本量 vs. Power
    #    扫描范围可根据 N_calc 动态调整
    n_min <- max(10, N_calc - 200)
    n_max <- N_calc + 200
    n_seq <- seq(n_min, n_max, by = 10)
    
    power_n <- sapply(n_seq, function(Ni) {
      tmp <- pwr::pwr.chisq.test(
        w         = w_value,
        df        = df_val,
        sig.level = alpha,
        N         = Ni
      )
      tmp$power
    })
    df_n <- data.frame(n_total = n_seq, power = power_n)
    
    p1 <- ggplot(df_n, aes(x = n_total, y = power)) +
      geom_line(color = "blue", size = 1) +
      ylim(0, 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      labs(
        x = "Total Sample Size",
        y = "Power",
        title = paste0("Power vs Sample Size\n(w = ", round(res$w, 3), ")")
      ) +
      theme_minimal(base_size = 14)
    
    output$prop_multi_power_curve <- renderPlot({
      p1
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  ####-###-###-###-###-###-###-###-###
  # (4) 相关系数检验----
  ####-###-###-###-###-###-###-###-###
  
  
  ##===-===-===-===-===-===-===
  ## (4.1) Single Correlation----
  ##===-===-===-===-===-===-===
  

  
  
  
  observeEvent(input$corr_one_calc, {
    
    alpha       <- input$corr_one_alpha
    power_req   <- input$corr_one_power
    r_val       <- input$corr_one_r
    alt         <- input$corr_one_alternative
    
    # pwr.r.test() 中，仅支持 "two.sided" 或 "less"/"greater" 被等价处理为 "one.sided"
    # 因此若用户选择 "less"/"greater"，我们就调用 "one.sided"
    alt_for_pwr <- if (alt == "two.sided") "two.sided" else "one.sided"
    
    # 1) 计算并显示结果
    res <- pwr::pwr.r.test(
      r           = r_val,
      sig.level   = alpha,
      power       = power_req,
      alternative = alt
    )
    
    # 取整后的样本量
    n_calc <- ceiling(res$n)
    
    # 2) 统一输出风格
    output$corr_one_result <- renderPrint({
      cat(
        "------------------------------------------------------\n",
        " Single Correlation Test (pwr::pwr.r.test)\n",
        "------------------------------------------------------\n",
        " approximate correlation power calculation (arctanh)\n\n",
        sprintf(" n            = %.2f\n", res$n),
        sprintf(" r            = %.3f\n", res$r),
        sprintf(" sig.level    = %.2f\n", res$sig.level),
        sprintf(" power        = %.2f\n", res$power),
        sprintf(" alternative  = %s\n",  res$alternative),
        "\nNOTE: 'n' is the total sample size needed.\n"
      )
    })
    
    # 3) 绘制两条功效曲线：
    #    (a) 样本量 n vs. Power
    #    (b) 相关系数 r vs. Power (此处把固定的 n=50 改为 n_calc)
    
    ## (a) 样本量 n vs. Power
    n_seq <- seq(5, 300, by = 5)
    power_n <- sapply(n_seq, function(n_i) {
      tmp <- pwr::pwr.r.test(
        r           = r_val,
        sig.level   = alpha,
        alternative = alt,
        n           = n_i
      )
      tmp$power
    })
    df_n <- data.frame(n = n_seq, power = power_n)
    
    p1 <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Sample Size (n)",
        y = "Power",
        title = "Power vs. Sample Size"
      ) +
      theme_minimal(base_size = 14)
    
    ## (b) 相关系数 r vs. Power
    r_seq <- seq(-0.9, 0.9, by = 0.1)
    power_r <- sapply(r_seq, function(rr) {
      tmp <- pwr::pwr.r.test(
        r           = rr,
        sig.level   = alpha,
        alternative = alt,
        n           = n_calc  # 使用计算得到的 n_calc
      )
      tmp$power
    })
    df_r <- data.frame(r = r_seq, power = power_r)
    
    p2 <- ggplot(df_r, aes(x = r, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Correlation (r)",
        y = "Power",
        title = paste("Power vs. Correlation\n(n =", n_calc, ")")
      ) +
      theme_minimal(base_size = 14)
    
    # 4) 并排输出
    output$corr_one_power_curve <- renderPlot({
      p1 + p2
    })
  })
  
  
  
  ##===-===-===-===-===-===-===
  ## (4.2) Two Correlations Comparison----
  ##===-===-===-===-===-===-===

  
  
  
  observeEvent(input$corr_two_calc, {
    
    alpha   <- input$corr_two_alpha
    power_r <- input$corr_two_power
    r1      <- input$corr_two_r1
    r2      <- input$corr_two_r2
    alt     <- input$corr_two_alternative
    
    # 1) 根据用户选择，决定单侧 or 双侧检验
    if (alt == "two.sided") {
      z_crit <- qnorm(1 - alpha/2)
    } else {
      z_crit <- qnorm(1 - alpha)
    }
    
    # Fisher's Z变换
    fisher_z <- function(r) {
      0.5 * log((1 + r) / (1 - r))
    }
    z1 <- fisher_z(r1)
    z2 <- fisher_z(r2)
    diff_z <- (z1 - z2)  # 真实备择下Z均值
    
    # 2) 定义根据给定n计算功效的函数
    calc_power_two_corr <- function(n) {
      # 方差 ~ 2/(n-3)，sd = sqrt(2/(n-3))
      se_diff <- sqrt(2 / (n - 3))
      z_mean  <- diff_z / se_diff
      
      if (alt == "two.sided") {
        # 双侧功效 = 左尾 + 右尾
        p_upper <- 1 - pnorm(z_crit, mean = z_mean, sd = 1)
        p_lower <- pnorm(-z_crit, mean = z_mean, sd = 1)
        p_upper + p_lower
      } else if (alt == "greater") {
        # r1 > r2
        1 - pnorm(z_crit, mean = z_mean, sd = 1)
      } else {
        # r1 < r2
        pnorm(-z_crit, mean = z_mean, sd = 1)
      }
    }
    
    # 3) 枚举 n=5..500，找到首个满足功效 >= power_r 的 n
    max_n <- 500
    found_n <- NA
    for (n_val in 5:max_n) {
      pwr_val <- calc_power_two_corr(n_val)
      if (pwr_val >= power_r) {
        found_n <- n_val
        break
      }
    }
    
    # 4) 输出(统一风格)
    output$corr_two_result <- renderPrint({
      cat(
        "--------------------------------------------------------------\n",
        " Two-Sample Correlation Test (via Fisher's Z Transformation)\n",
        "--------------------------------------------------------------\n",
        sprintf(" r1 = %.3f , r2 = %.3f\n", r1, r2),
        sprintf(" Desired power = %.2f , alpha = %.2f , alternative: %s\n\n", 
                power_r, alpha, alt)
      )
      
      if (is.na(found_n)) {
        cat(" No sample size up to", max_n,
            "achieves the desired power under these assumptions.\n")
      } else {
        cat(sprintf(
          " Suggested sample size (each group) = %d to achieve power ~ %.2f or higher.\n",
          found_n, power_r
        ))
        cat(sprintf(
          " Power at n = %d is approximately: %.3f\n",
          found_n, calc_power_two_corr(found_n)
        ))
      }
      
      cat(
        "\n(Using simplified Fisher's Z method for two-sample correlation comparison.)\n"
      )
    })
    
    # 5) 绘图
    # (a) p1: Power vs. Sample Size
    #     横轴区间 => [found_n - 200, found_n + 200] 做好边界保护
    if (is.na(found_n)) {
      # 若没找到合适n, 就简单从5..300
      n_seq <- seq(5, 300, by = 5)
    } else {
      n_min <- max(5, found_n - 200)
      n_max <- found_n + 200
      n_seq <- seq(n_min, n_max, by = 5)
    }
    
    power_seq <- sapply(n_seq, calc_power_two_corr)
    df_n <- data.frame(n = n_seq, power = power_seq)
    
    p1 <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_r, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Sample Size (per group)",
        y = "Power",
        title = "Power vs. Sample Size"
      ) +
      theme_minimal(base_size = 14)
    
    # (b) p2: Power vs. r2
    #     原先示例固定n=50, 现在改为 found_n (若没找到则用 50)
    if (is.na(found_n)) {
      n_for_r2 <- 50
    } else {
      n_for_r2 <- found_n
    }
    
    r2_seq <- seq(-0.9, 0.9, by = 0.1)
    power_r2 <- sapply(r2_seq, function(rr2) {
      # 只要让 diff_z动态改变即可
      diff_z_dyn <- fisher_z(r1) - fisher_z(rr2)
      se_diff <- sqrt(2 / (n_for_r2 - 3))
      z_mean  <- diff_z_dyn / se_diff
      
      if (alt == "two.sided") {
        p_upper <- 1 - pnorm(z_crit, mean = z_mean, sd = 1)
        p_lower <- pnorm(-z_crit, mean = z_mean, sd = 1)
        p_upper + p_lower
      } else if (alt == "greater") {
        1 - pnorm(z_crit, mean = z_mean, sd = 1)
      } else {
        pnorm(-z_crit, mean = z_mean, sd = 1)
      }
    })
    df_r2 <- data.frame(r2 = r2_seq, power = power_r2)
    
    p2 <- ggplot(df_r2, aes(x = r2, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_r, color = "red", linetype = "dashed") +
      ylim(0, 1) +
      labs(
        x = "Correlation 2 (r2)",
        y = "Power",
        title = paste("Power vs. r2\n(n =", n_for_r2, ", r1 =", r1, ")")
      ) +
      theme_minimal(base_size = 14)
    
    # 6) 并排输出
    output$corr_two_power_curve <- renderPlot({
      p1 + p2
    })
  })
  
  
  
  
  ####-###-###-###-###-###-###-###-###
  # (5) 流行病学研究----
  ####-###-###-###-###-###-###-###-###
  
  ##===-===-===-===-===-===-===
  ## (5.1) Case-Control----
  ##===-===-===-===-===-===-===
  observeEvent(input$epir_cc_calc, {
    alpha_val <- input$epir_cc_alpha
    power_req <- input$epir_cc_power
    or_val    <- input$epir_cc_or
    p0_val    <- input$epir_cc_p0
    r_val     <- input$epir_cc_r
    
    # 1) “给定 power -> 求 n”
    res_cc <- epiR::epi.sscc(
      OR         = or_val,
      p0         = p0_val,
      n          = NA,
      power      = power_req,
      r          = r_val,
      design     = 1,
      sided.test = 2,
      conf.level = 1 - alpha_val
    )
    
    
    
    
    # 简化文本输出
    output$epir_cc_result <- renderPrint({
      cat("Sample size calculation result for Case-Control study:\n")
      cat("-----------------------------------\n")
      cat(sprintf("Total sample size: %d\n", res_cc$n.total))
      cat(sprintf("Cases: %d\n", res_cc$n.case))
      cat(sprintf("Controls: %d\n", res_cc$n.control))
      cat(sprintf("Power: %.2f\n", res_cc$power))
      cat(sprintf("Assumed Odds Ratio (OR): %.2f\n", res_cc$OR))
      cat("-----------------------------------\n")
    })
    
    
    
    ##绘图逻辑
  
    
    n_default <- 100                     # 固定样本量 (用于 OR vs. Power)
    
    ## === 功效 vs 样本量 ===
    # 样本量范围
    n_seq <- seq(20, 400, by = 20)
    # 计算每个样本量下的功效
    power_n <- sapply(n_seq, function(n) {
      res <- epiR::epi.sscc(
        OR         = or_val,
        p0         = p0_val,
        n          = n,              # 当前样本量
        power      = NA,             # 不指定功效，函数自动计算
        r          = r_val,
        design     = 1,              # 设计效应
        sided.test = 2,              # 双侧检验
        conf.level = 1 - alpha_val   # 显著性水平
      )
      res$power  # 提取计算的功效
    })
    # 数据框用于绘图
    df_n <- data.frame(n = n_seq, power = power_n)
    
    # 绘制 Power vs. Sample Size 图
    p1_plot <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") + # 目标功效
      ylim(0, 1) +
      labs(
        title = "Power vs. Sample Size",
        x = "Cases + Controls",
        y = "Power"
      ) +
      theme_minimal(base_size = 14)
    
    ## === 功效 vs 优势比 ===
    # 优势比范围
    or_seq <- seq(1.1, 5, by = 0.4)
    # 计算每个优势比下的功效
    power_or <- sapply(or_seq, function(o) {
      res <- epiR::epi.sscc(
        OR         = o,              # 当前优势比
        p0         = p0_val,         # 对照组暴露率
        n          = n_default,      # 固定样本量
        power      = NA,             # 不指定功效，函数自动计算
        r          = r_val,
        design     = 1,              # 设计效应
        sided.test = 2,              # 双侧检验
        conf.level = 1 - alpha_val   # 显著性水平
      )
      res$power  # 提取计算的功效
    })
    # 数据框用于绘图
    df_or <- data.frame(OR = or_seq, power = power_or)
    
    # 绘制 Power vs. OR 图
    p2_plot <- ggplot(df_or, aes(x = OR, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") + # 目标功效
      ylim(0, 1) +
      labs(
        title = paste("Power vs. OR\n(n =", n_default, ")"),
        x = "Odds Ratio",
        y = "Power"
      ) +
      theme_minimal(base_size = 14)
    
    ## === 合并两幅图 ===
    # 使用 cowplot 拼接图像
    library(cowplot)
    combined_plot <- plot_grid(
      p1_plot, p2_plot,                # 两个图像
      labels = c("A", "B"),            # 标注子图（A 和 B）
      ncol = 2                         # 两列布局
    )
    
    # 输出合并图像
    output$epir_cc_power_plot <- renderPlot({
      combined_plot
    })
  })
  
  
  
  
  
  
  
  
  ##===-===-===-===-===-===-===
  ## (5.2) Cohort (RR)----
  ##===-===-===-===-===-===-===
 
  observeEvent(input$epir_cohort_rr_calc, {
    alpha_val <- input$epir_cohort_rr_alpha   # 显著性水平 (α)
    power_req <- input$epir_cohort_rr_power   # 功效 (1 - β)
    rr_val    <- input$epir_cohort_rr_val     # 相对风险 (RR)
    p0_val    <- input$epir_cohort_rr_p0      # 基线风险 (p0)
    r_val     <- input$epir_cohort_rr_ratio   # 暴露组与非暴露组样本比例
    
    # 1) “给定 power -> 求 n”
    res_cohort <- epiR::epi.sscohortt(
      irexp1     = p0_val * rr_val,           # 暴露组事件发生率
      irexp0     = p0_val,                    # 非暴露组事件发生率
      n          = NA,                        # 样本量未知
      power      = power_req,                 # 功效
      r          = r_val,                     # 样本比例
      design     = 1,                         # 设计效应
      sided.test = 2,                         # 双侧检验
      conf.level = 1 - alpha_val              # 显著性水平
    )
    
    # 简化文本输出
    output$epir_cohort_rr_result <- renderPrint({
      cat("Sample size calculation result for Cohort study (Relative Risk):\n")
      cat("-----------------------------------\n")
      cat(sprintf("Total sample size: %d\n", res_cohort$n.total))
      cat(sprintf("Exposed group: %d\n", res_cohort$n.exp1))
      cat(sprintf("Non-exposed group: %d\n", res_cohort$n.exp0))
      cat(sprintf("Power: %.2f\n", res_cohort$power))
      cat(sprintf("Assumed Relative Risk (RR): %.2f\n", res_cohort$irr))
      cat("-----------------------------------\n")
    })
    
    
    ## 绘图逻辑
    n_default <- 100                          # 固定样本量 (用于 RR vs. Power)
    
    ## === 功效 vs 样本量 ===
    # 样本量范围
    n_seq <- seq(20, 400, by = 20)
    # 计算每个样本量下的功效
    power_n <- sapply(n_seq, function(n) {
      res <- epiR::epi.sscohortt(
        irexp1     = p0_val * rr_val,         # 暴露组事件发生率
        irexp0     = p0_val,                  # 非暴露组事件发生率
        n          = n,                       # 当前样本量
        power      = NA,                      # 不指定功效，函数自动计算
        r          = r_val,
        design     = 1,
        sided.test = 2,
        conf.level = 1 - alpha_val
      )
      res$power  # 提取计算的功效
    })
    # 数据框用于绘图
    df_n <- data.frame(n = n_seq, power = power_n)
    
    # 绘制 Power vs. Sample Size 图
    p1_plot <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") + # 目标功效
      ylim(0, 1) +
      labs(
        title = "Power vs. Sample Size",
        x = "Sample Size (Total)",
        y = "Power"
      ) +
      theme_minimal(base_size = 14)
    
    ## === 功效 vs 相对风险 ===
    # 相对风险范围
    rr_seq <- seq(1.1, 5, by = 0.4)
    # 计算每个相对风险下的功效
    power_rr <- sapply(rr_seq, function(rr) {
      res <- epiR::epi.sscohortt(
        irexp1     = p0_val * rr,             # 暴露组事件发生率
        irexp0     = p0_val,                  # 非暴露组事件发生率
        n          = n_default,               # 固定样本量
        power      = NA,                      # 不指定功效，函数自动计算
        r          = r_val,
        design     = 1,
        sided.test = 2,
        conf.level = 1 - alpha_val
      )
      res$power  # 提取计算的功效
    })
    # 数据框用于绘图
    df_rr <- data.frame(RR = rr_seq, power = power_rr)
    
    # 绘制 Power vs. Relative Risk 图
    p2_plot <- ggplot(df_rr, aes(x = RR, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") + # 目标功效
      ylim(0, 1) +
      labs(
        title = paste("Power vs. RR\n(n =", n_default, ")"),
        x = "Relative Risk (RR)",
        y = "Power"
      ) +
      theme_minimal(base_size = 14)
    
    ## === 合并两幅图 ===
    # 使用 cowplot 拼接图像
    library(cowplot)
    combined_plot <- plot_grid(
      p1_plot, p2_plot,                # 两个图像
      labels = c("A", "B"),            # 标注子图（A 和 B）
      ncol = 2                         # 两列布局
    )
    
    # 输出合并图像
    output$epir_cohort_rr_plot <- renderPlot({
      combined_plot
    })
  })
  
  
  
  
  
  
  
  
  ##===-===-===-===-===-===-===
  ## (5.3) Cohort (Known P1, P2) ----
  ##===-===-===-===-===-===-===

  observeEvent(input$epir_cohort_p1p2_calc, {
    alpha_val <- input$epir_cohort_p1p2_alpha   # 显著性水平 (α)
    power_req <- input$epir_cohort_p1p2_power   # 功效 (1 - β)
    p1_val    <- input$epir_cohort_p1p2_p1      # 暴露组发生风险 (P1)
    p2_val    <- input$epir_cohort_p1p2_p2      # 非暴露组发生风险 (P2)
    r_val     <- input$epir_cohort_p1p2_ratio   # 暴露组与非暴露组样本比例
    
    # 1) “给定 power -> 求 n”
    res_cohort <- epiR::epi.sscohortc(
      irexp1     = p1_val,                      # 暴露组事件发生风险
      irexp0     = p2_val,                      # 非暴露组事件发生风险
      n          = NA,                          # 样本量未知
      power      = power_req,                   # 功效
      r          = r_val,                       # 样本比例
      design     = 1,                           # 设计效应
      sided.test = 2,                           # 双侧检验
      conf.level = 1 - alpha_val                # 显著性水平
    )
    
    # 提取计算的样本量结果
    n_calculated <- res_cohort$n.total
    
    # 简化文本输出
    output$epir_cohort_p1p2_result <- renderPrint({
      cat("Sample size calculation result for Cohort study (p1, p2 input):\n")
      cat("-----------------------------------\n")
      cat(sprintf("Total sample size: %d\n", res_cohort$n.total))
      cat(sprintf("Exposed group: %d\n", res_cohort$n.exp1))
      cat(sprintf("Non-exposed group: %d\n", res_cohort$n.exp0))
      cat(sprintf("Power: %.2f\n", res_cohort$power))
      cat(sprintf("Assumed Risk Ratio (RR): %.2f\n", res_cohort$irr))
      cat("-----------------------------------\n")
    })
    
    
    ## 绘图逻辑
    n_default <- n_calculated  # 动态调整默认样本量
    
    ## === 功效 vs 样本量 ===
    # 样本量范围动态调整为 n_calculated ± 200
    n_seq <- seq(max(20, n_calculated - 200), n_calculated + 200, by = 20)
    
    # 计算每个样本量下的功效
    power_n <- sapply(n_seq, function(n) {
      res <- epiR::epi.sscohortc(
        irexp1     = p1_val,                  # 暴露组事件发生风险
        irexp0     = p2_val,                  # 非暴露组事件发生风险
        n          = n,                       # 当前样本量
        power      = NA,                      # 不指定功效，函数自动计算
        r          = r_val,
        design     = 1,
        sided.test = 2,
        conf.level = 1 - alpha_val
      )
      res$power  # 提取计算的功效
    })
    # 数据框用于绘图
    df_n <- data.frame(n = n_seq, power = power_n)
    
    # 绘制 Power vs. Sample Size 图
    p1_plot <- ggplot(df_n, aes(x = n, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") + # 目标功效
      ylim(0, 1) +
      labs(
        title = "Power vs. Sample Size",
        x = "Sample Size (Total)",
        y = "Power"
      ) +
      theme_minimal(base_size = 14)
    
    ## === 功效 vs 风险比 ===
    # 风险比范围
    rr_seq <- seq(1.1, 5, by = 0.4)
    # 计算每个风险比下的功效
    power_rr <- sapply(rr_seq, function(rr) {
      res <- epiR::epi.sscohortc(
        irexp1     = p2_val * rr,             # 暴露组事件发生风险 (计算得出)
        irexp0     = p2_val,                  # 非暴露组事件发生风险
        n          = n_default,               # 固定样本量
        power      = NA,                      # 不指定功效，函数自动计算
        r          = r_val,
        design     = 1,
        sided.test = 2,
        conf.level = 1 - alpha_val
      )
      res$power  # 提取计算的功效
    })
    # 数据框用于绘图
    df_rr <- data.frame(RR = rr_seq, power = power_rr)
    
    # 绘制 Power vs. Risk Ratio 图
    p2_plot <- ggplot(df_rr, aes(x = RR, y = power)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = power_req, color = "red", linetype = "dashed") + # 目标功效
      ylim(0, 1) +
      labs(
        title = paste("Power vs. Risk Ratio\n(n =", n_default, ")"),
        x = "Risk Ratio (RR)",
        y = "Power"
      ) +
      theme_minimal(base_size = 14)
    
    ## === 合并两幅图 ===
    # 使用 cowplot 拼接图像
    library(cowplot)
    combined_plot <- plot_grid(
      p1_plot, p2_plot,                # 两个图像
      labels = c("A", "B"),            # 标注子图（A 和 B）
      ncol = 2                         # 两列布局
    )
    
    # 输出合并图像
    output$epir_cohort_p1p2_plot <- renderPlot({
      combined_plot
    })
  })
  
  
  
  ####-###-###-###-###-###-###-###-###
  # (6) 非劣效 / 等效 / 优效性试验类----
  ####-###-###-###-###-###-###-###-###
  
  ##===-===-===-===-===-===-===
  ## (6.1) (A) Non-Inferiority----
  ##===-===-===-===-===-===-===
  
  ###────────────────────────────────────
  ### A.1 Two Means----
  ###────────────────────────────────────
 
  observeEvent(input$ni_means_calc, {
    # 获取用户输入
    alpha  <- input$ni_means_alpha    # 显著性水平 (α)
    sigma  <- input$ni_means_sigma    # 标准差
    k      <- input$ni_means_ratio    # 样本量比例
    delta  <- input$ni_means_margin   # 非劣效界值
    m_diff <- input$ni_means_diff     # 真差 (μ2-μ1)
    power_target <- input$ni_means_power  # 功效目标值 (Power)
    beta_target <- 1 - power_target      # 目标 beta 值
    
    # 调用 TrialSize::TwoSampleMean.NIS 计算样本量（满足目标功效值）
    target_sample_size <- TrialSize::TwoSampleMean.NIS(
      alpha  = alpha,
      beta   = beta_target,  # 对应目标功效
      sigma  = sigma,
      k      = k,
      delta  = delta,
      margin = m_diff
    )
    
    # 调用 TrialSize::TwoSampleMean.NIS 计算样本量（功效为 50%）
    base_sample_size <- TrialSize::TwoSampleMean.NIS(
      alpha  = alpha,
      beta   = 0.5,  # 对应功效为 50%
      sigma  = sigma,
      k      = k,
      delta  = delta,
      margin = m_diff
    )
    
    # 输出计算结果
    output$ni_means_result <- renderPrint({
      cat("Sample size calculation for Non-Inferiority Test (Two Means):\n")
      cat("-----------------------------------\n")
      cat(sprintf("Significance level (α): %.3f\n", alpha))
      cat(sprintf("Standard deviation (σ): %.2f\n", sigma))
      cat(sprintf("Allocation ratio (k): %.2f\n", k))
      cat(sprintf("NI margin (δ): %.2f\n", delta))
      cat(sprintf("True mean difference (μ2 - μ1): %.2f\n", m_diff))
      cat(sprintf("Target power: %.3f\n", power_target))
      cat("-----------------------------------\n")
      cat(sprintf("Sample size (Power = 50%%): %.1f\n", base_sample_size))
      cat(sprintf("Sample size (Target power): %.1f\n", target_sample_size))
    })
    
    

    
    # 功效曲线绘制逻辑
    n_seq <- seq(50, target_sample_size + 500, by = 50)  # 样本量范围
    power_curve <- sapply(n_seq, function(n) {
      SE <- sqrt(sigma^2 * (1 + 1 / k) / n)  # 样本量对应的标准误
      EffectSize <- (delta - m_diff) / SE    # 效应大小
      Power <- pnorm(EffectSize - qnorm(1 - alpha))  # 单侧显著性水平
      return(Power)
    })
    
    # 构造数据框用于绘图
    df_power <- data.frame(SampleSize = n_seq, Power = power_curve)
    
    # 绘制功效曲线
    output$ni_means_plot <- renderPlot({
      library(ggplot2)
      ggplot(df_power, aes(x = SampleSize, y = Power)) +
        geom_line(color = "blue", linewidth = 1) +  # 功效曲线
        geom_hline(yintercept = power_target, color = "red", linetype = "dashed") +  # 功效目标线
        geom_vline(xintercept = base_sample_size, color = "green", linetype = "dashed") +  # 满足功效 50% 样本量线
        geom_vline(xintercept = target_sample_size, color = "purple", linetype = "dashed") +  # 满足目标功效值样本量线
        ylim(0, 1) +
        labs(
          x = "Sample Size (n)",
          y = "Power",
          title = "Sample Size vs. Power"
        ) +
        theme_minimal(base_size = 14) +
        annotate("text", x = base_sample_size, y = 0.1, label = "Power=50%", color = "green", angle = 90, vjust = -0.5) +
        annotate("text", x = target_sample_size, y = 0.1, label = "Target power", color = "purple", angle = 90, vjust = -0.5)
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  ###────────────────────────────────────
  ### A.2 Two Proportions (Difference)----
  ###────────────────────────────────────
  
 
  observeEvent(input$ni_prop_calc, {
    # 获取用户在 UI 中的输入
    alpha        <- input$ni_prop_alpha     # 显著性水平 (α)
    power_target <- input$ni_prop_power     # 目标功效 (1 - β)
    p1           <- input$ni_prop_p1        # 检验组事件发生率 (Treatment)
    p2           <- input$ni_prop_p2        # 对照组事件发生率 (Control)
    margin       <- input$ni_prop_margin    # 非劣效(或超级)界值 (Δ)
    k            <- input$ni_prop_ratio     # 样本量配比 n1/n2
    
    # beta 对应 1 - power_target
    beta_target  <- 1 - power_target
    
    # 差值 delta = p1 - p2
    delta <- p1 - p2
    
    # 使用 TrialSize::TwoSampleProportion.NIS 计算所需样本量
    # base_sample_size 对应功效 = 50%
    # target_sample_size 对应目标功效 = power_target
    base_sample_size <- TrialSize::TwoSampleProportion.NIS(
      alpha  = alpha,
      beta   = 0.50,     # 对应功效 50%
      p1     = p1,
      p2     = p2,
      k      = k,
      delta  = delta,
      margin = margin
    )
    
    target_sample_size <- TrialSize::TwoSampleProportion.NIS(
      alpha  = alpha,
      beta   = beta_target,  # 对应目标功效
      p1     = p1,
      p2     = p2,
      k      = k,
      delta  = delta,
      margin = margin
    )
    
    # 将结果输出到文本区域
    output$ni_prop_result <- renderPrint({
      cat("Sample size calculation for Non-Inferiority Test (Two Proportions - Difference):\n")
      cat("-------------------------------------------\n")
      cat(sprintf("Significance level (α): %.3f\n", alpha))
      cat(sprintf("Proportion in treatment group (p1): %.3f\n", p1))
      cat(sprintf("Proportion in control group (p2): %.3f\n", p2))
      cat(sprintf("Allocation ratio (k = n1/n2): %.3f\n", k))
      cat(sprintf("NI margin (Δ): %.3f\n", margin))
      cat(sprintf("Observed difference (p1 - p2): %.3f\n", delta))
      cat(sprintf("Target power: %.3f\n", power_target))
      cat("-------------------------------------------\n")
      cat(sprintf("Sample size (Power = 50%%): %.1f\n", base_sample_size))
      cat(sprintf("Sample size (Target power): %.1f\n", target_sample_size))
    })
    
    
    
  })
  
  
  
  ###────────────────────────────────────
  ### A.3 Two Proportions (Ratio)----
  ###────────────────────────────────────
  
  
  observeEvent(input$ni_ratio_calc, {
    # 1) 获取用户输入
    alpha  <- input$ni_ratio_alpha    # 0.025
    power  <- input$ni_ratio_power    # 0.80
    p_t    <- input$ni_ratio_p1       # 处理组
    p_c    <- input$ni_ratio_p2       # 对照组
    margin <- input$ni_ratio_rmargin  # 非劣效界值 (e.g. 1.25)
    k      <- input$ni_ratio_alloc
    beta   <- 1 - power
    
    # 2) 计算比值 or_val (如同原逻辑)
    or_val <- p_t * (1 - p_c) / ( p_c * (1 - p_t) )
    
    # 3) 分别计算：功效=50% 与 目标功效的所需样本量
    base_sample_size <- TrialSize::RelativeRisk.NIS(
      alpha  = alpha,
      beta   = 0.50,    # => power=0.50
      or     = or_val,
      k      = k,
      pt     = p_t,
      pc     = p_c,
      margin = margin
    )
    
    target_sample_size <- TrialSize::RelativeRisk.NIS(
      alpha  = alpha,
      beta   = beta,
      or     = or_val,
      k      = k,
      pt     = p_t,
      pc     = p_c,
      margin = margin
    )
    
    # 4) 输出
    output$ni_ratio_result <- renderPrint({
      cat("Sample size calculation for Non-Inferiority Test (Two Proportions - Ratio):\n")
      cat("-------------------------------------------\n")
      cat(sprintf("Significance level (α): %.3f\n", alpha))
      cat(sprintf("Proportion in treatment group (p1): %.3f\n", p_t))
      cat(sprintf("Proportion in control group (p2): %.3f\n", p_c))
      cat(sprintf("Allocation ratio (k = n1/n2): %.3f\n", k))
      cat(sprintf("NI margin (RR upper limit): %.3f\n", margin))
      cat(sprintf("Calculated odds ratio: %.3f\n", or_val))
      cat(sprintf("Target power: %.3f\n", power))
      cat("-------------------------------------------\n")
      cat(sprintf("Sample size (Power = 50%%): %.1f\n", base_sample_size))
      cat(sprintf("Sample size (Target power): %.1f\n", target_sample_size))
    })
    
    
    
  })
  
  
  
  
  
  ##===-===-===-===-===-===-===
  ##  (6.2) (B) Equivalence ----
  ##===-===-===-===-===-===-===
  
  ###────────────────────────────────────
  ### B.1 Two Means ----
  ###────────────────────────────────────

  observeEvent(input$eq_means_calc, {
    # 1) 读取用户输入
    alpha  <- input$eq_means_alpha   # 显著性水平
    power  <- input$eq_means_power   # 目标检验功效
    sigma  <- input$eq_means_sigma   # 标准差
    k      <- input$eq_means_ratio   # k = n1 / n2
    delta  <- input$eq_means_delta   # 等效界值(± delta)
    margin <- input$eq_means_margin  # 真差 (μ2 - μ1)
    beta   <- 1 - power              # (1 - power)
    
    # 2) 用 TrialSize 计算 n1
    #    （TrialSize返回的是 n1，另一组 n2 = n1/k）
    target_sample_size <- TrialSize::TwoSampleMean.Equivalence(
      alpha  = alpha,
      beta   = beta,
      sigma  = sigma,
      k      = k,
      delta  = delta,
      margin = margin
    )
    
    # 另算一个“功效=50%”时的 n1
    base_sample_size <- TrialSize::TwoSampleMean.Equivalence(
      alpha  = alpha,
      beta   = 0.5,   # => power=0.5
      sigma  = sigma,
      k      = k,
      delta  = delta,
      margin = margin
    )
    
    # 3) 输出 (统一风格)
    output$eq_means_result <- renderPrint({
      cat("Sample size calculation for Equivalence Test (Two Means):\n")
      cat("--------------------------------------------------\n")
      cat(sprintf("Significance level (α): %.3f\n", alpha))
      cat(sprintf("Standard deviation (σ): %.2f\n", sigma))
      cat(sprintf("Allocation ratio (k = n1/n2): %.2f\n", k))
      cat(sprintf("Equivalence margin (±δ): %.2f\n", delta))
      cat(sprintf("True difference (μ2 - μ1): %.2f\n", margin))
      cat(sprintf("Target power: %.3f\n", power))
      cat("--------------------------------------------------\n")
      cat(sprintf("Sample size (Power = 50%%): %.1f\n", base_sample_size))
      cat(sprintf("Sample size (Target power): %.1f\n", target_sample_size))
    })
    
    
    # 4) 定义一个函数，给定 n1，按同一公式反解 power
    eq_power_calc <- function(n1) {
      # 若 n1 无效返回 NA
      if (n1 < 1) return(NA_real_)
      
      # 以 TrialSize 源码为准 => z_{1-alpha}, z_{1-beta/2}
      # 这里要反过来求 beta => power
      z_a <- qnorm(1 - alpha)  # 注意：源码是 qnorm(1 - alpha), 非 alpha/2
      X <- sqrt( ((delta - abs(margin))^2 * (n1/k)) / ( sigma^2*(1 + 1/k) ) )
      
      pwr <- 2 * pnorm(X - z_a) - 1
      # pwr 可能出现<0或>1的极端情况，可夹逼到[0, 1]
      if (pwr < 0) pwr <- 0
      if (pwr > 1) pwr <- 1
      return(pwr)
    }
    
    # 5) 扫描 n1 并绘制功效曲线
    #    起点可从 2 或 5 开始，避免 sqrt(1/n1) 过大
    #    终点挑 target_sample_size + 一段范围
    n_min <- max(5, floor(base_sample_size/2))
    n_max <- ceiling(target_sample_size + 300)
    n_seq <- seq(n_min, n_max, by = 20)
    
    power_curve <- sapply(n_seq, eq_power_calc)
    df_power <- data.frame(n1 = n_seq, Power = power_curve)
    
    # 6) 绘图
    output$eq_means_plot <- renderPlot({
      library(ggplot2)
      ggplot(df_power, aes(x = n1, y = Power)) +
        geom_line(color = "blue", size = 1) +
        # 标示目标功效(红线) => y = power
        geom_hline(yintercept = power, color = "red", linetype = "dashed") +
        # 标示 base_sample_size (绿线) => 功效=50%
        geom_vline(xintercept = base_sample_size, color = "green", linetype = "dashed") +
        # 标示 target_sample_size (紫线)
        geom_vline(xintercept = target_sample_size, color = "purple", linetype = "dashed") +
        ylim(0, 1) +
        labs(
          x = "Sample Size n1",
          y = "Power",
          title = "n1 vs. Power"
        ) +
        theme_minimal(base_size = 14) +
        annotate("text",
                 x = base_sample_size, y = 0.1,
                 label = "power=50%", color = "green",
                 angle = 90, vjust = -0.5) +
        annotate("text",
                 x = target_sample_size, y = 0.1,
                 label = "Target power", color = "purple",
                 angle = 90, vjust = -0.5)
    })
  })
  
  
  
  
  ###────────────────────────────────────
  ### B.2 Two Proportions (Difference)----
  ###────────────────────────────────────
  observeEvent(input$eq_prop_calc, {
    # 1) 获取用户输入
    alpha  <- input$eq_prop_alpha
    power  <- input$eq_prop_power
    p1     <- input$eq_prop_p1
    p2     <- input$eq_prop_p2
    k      <- input$eq_prop_ratio
    delta  <- input$eq_prop_delta   # p1 - p2?
    margin <- input$eq_prop_margin  # 等效界值
    beta   <- 1 - power
    
    # 2) 分别计算：功效=50% 与 目标功效的所需样本量
    base_sample_size <- TrialSize::TwoSampleProportion.Equivalence(
      alpha  = alpha,
      beta   = 0.50,  # => power=0.50
      p1     = p1,
      p2     = p2,
      k      = k,
      delta  = delta,
      margin = margin
    )
    
    target_sample_size <- TrialSize::TwoSampleProportion.Equivalence(
      alpha  = alpha,
      beta   = beta,
      p1     = p1,
      p2     = p2,
      k      = k,
      delta  = delta,
      margin = margin
    )
    
    # 3) 输出
    output$eq_prop_result <- renderPrint({
      cat("Sample size calculation for Equivalence Test (Two Proportions - Difference):\n")
      cat("-------------------------------------------\n")
      cat(sprintf("Significance level (α): %.3f\n", alpha))
      cat(sprintf("Proportion in treatment group (p1): %.3f\n", p1))
      cat(sprintf("Proportion in control group (p2): %.3f\n", p2))
      cat(sprintf("Allocation ratio (k = n1/n2): %.3f\n", k))
      cat(sprintf("Equivalence margin (δ): %.3f\n", margin))
      cat(sprintf("Observed difference (p1 - p2): %.3f\n", delta))
      cat(sprintf("Target power: %.3f\n", power))
      cat("-------------------------------------------\n")
      cat(sprintf("Sample size (Power = 50%%): %.1f\n", base_sample_size))
      cat(sprintf("Sample size (Target power): %.1f\n", target_sample_size))
    })
    

  })
  
  
  
  
  ###────────────────────────────────────
  ### B.3 Two Proportions (Ratio)----
  ###────────────────────────────────────
  observeEvent(input$eq_ratio_calc, {
    # 1) 获取用户输入
    alpha   <- input$eq_ratio_alpha
    power   <- input$eq_ratio_power
    pt      <- input$eq_ratio_pt
    pc      <- input$eq_ratio_pc
    k       <- input$eq_ratio_alloc
    margin  <- input$eq_ratio_margin
    beta    <- 1 - power
    
    # 2) 先计算 or_val
    or_val <- pt*(1-pc)/( pc*(1-pt) )
    
    # 3) 分别计算：功效=50% 与 目标功效的所需样本量
    base_sample_size <- TrialSize::RelativeRisk.Equivalence(
      alpha  = alpha,
      beta   = 0.50,   # => power=0.50
      or     = or_val,
      k      = k,
      pt     = pt,
      pc     = pc,
      margin = margin
    )
    
    target_sample_size <- TrialSize::RelativeRisk.Equivalence(
      alpha  = alpha,
      beta   = beta,
      or     = or_val,
      k      = k,
      pt     = pt,
      pc     = pc,
      margin = margin
    )
    
    # 4) 输出
    output$eq_ratio_result <- renderPrint({
      cat("Sample size calculation for Equivalence Test (Two Proportions - Ratio):\n")
      cat("-------------------------------------------\n")
      cat(sprintf("Significance level (α): %.3f\n", alpha))
      cat(sprintf("Proportion in treatment group (p1): %.3f\n", pt))
      cat(sprintf("Proportion in control group (p2): %.3f\n", pc))
      cat(sprintf("Allocation ratio (k = n1/n2): %.3f\n", k))
      cat(sprintf("Equivalence margin (Δ): %.3f\n", margin))
      cat(sprintf("Calculated odds ratio: %.3f\n", or_val))
      cat(sprintf("Target power: %.3f\n", power))
      cat("-------------------------------------------\n")
      cat(sprintf("Sample size (Power = 50%%): %.1f\n", base_sample_size))
      cat(sprintf("Sample size (Target power): %.1f\n", target_sample_size))
    })
    

  })
  
  ##===-===-===-===-===-===-===
  ## (6.3) (C) Superiority (Equality)----
  ##===-===-===-===-===-===-===
  
  ###────────────────────────────────────
  ### C.1 Two Means----
  ###────────────────────────────────────
  
  
  
  observeEvent(input$sup_means_calc, {
    # 1) 获取用户输入
    alpha  <- input$sup_means_alpha   # 显著性水平(双侧)
    power  <- input$sup_means_power   # 目标功效
    sigma  <- input$sup_means_sigma   # 标准差
    k      <- input$sup_means_ratio   # k = n1 / n2
    margin <- input$sup_means_margin  # 要检测的差值
    beta   <- 1 - power
    
    # 2) 用 TrialSize::TwoSampleMean.Equality 计算样本量
    #    注意：它返回的是 n1 (实验组的样本量)，另一组为 n2 = n1/k
    target_sample_size <- TrialSize::TwoSampleMean.Equality(
      alpha  = alpha,
      beta   = beta,
      sigma  = sigma,
      k      = k,
      margin = margin
    )
    
    # 另算一个“功效=50%”时的 n1
    base_sample_size <- TrialSize::TwoSampleMean.Equality(
      alpha  = alpha,
      beta   = 0.5,  # => power=0.5
      sigma  = sigma,
      k      = k,
      margin = margin
    )
    
    # 3) 输出（统一风格）
    output$sup_means_result <- renderPrint({
      cat("Sample size calculation for Superiority Test (Two Means):\n")
      cat("--------------------------------------------------\n")
      cat(sprintf("Significance level (α): %.3f\n", alpha))
      cat(sprintf("Standard deviation (σ): %.2f\n", sigma))
      cat(sprintf("Allocation ratio (k = n1/n2): %.2f\n", k))
      cat(sprintf("True difference (μ2 - μ1): %.2f\n", margin))
      cat(sprintf("Target power: %.3f\n", power))
      cat("--------------------------------------------------\n")
      cat(sprintf("Sample size (Power = 50%%): %.1f\n", base_sample_size))
      cat(sprintf("Sample size (Target power): %.1f\n", target_sample_size))
    })
    
    
    # 4) 绘图逻辑：使用与 TrialSize 同样的公式，但反推功效
    #    TrialSize::TwoSampleMean.Equality 的内部公式：
    #      n2 = ( z_{1-α/2} + z_{1-β} )^2 * sigma^2 * (1 + 1/k) / margin^2
    #      n1 = k * n2
    #    若给定 n1，计算 power(约等于)：
    #      n2 = n1 / k
    #      => (z_{1-α/2} + z_{1-β})^2 = (n2 * margin^2)/( sigma^2 * (1+1/k) )
    #      => z_{1-β} = sqrt(...) - z_{1-α/2}
    #      => 1-β = pnorm( sqrt(...) - z_{1-α/2} ), 即 power = pnorm(...)
    sup_power_calc <- function(n1) {
      # 若 n1 无效/过小 返回 NA
      if (n1 < 1) return(NA_real_)
      
      z_half_alpha <- qnorm(1 - alpha/2)   # 双侧 => alpha/2 each tail
      
      n2 <- n1 / k
      # X = sqrt( ( n2 * margin^2 ) / ( sigma^2*(1+1/k) ) )
      # power = pnorm( X - z_{1-α/2} )
      X <- sqrt( (n2 * margin^2)/( sigma^2*(1 + 1/k) ) )
      pwr_value <- pnorm(X - z_half_alpha)
      
      # 夹逼在 [0,1] 避免数值波动
      pwr_value <- max(0, min(pwr_value, 1))
      return(pwr_value)
    }
    
    # 5) 对 n1 做枚举，绘制功效曲线
    n_min <- max(5, floor(base_sample_size/2))
    n_max <- ceiling(target_sample_size + 300)
    if (n_min < 5) n_min <- 5
    
    n_seq <- seq(n_min, n_max, by = 20)
    power_seq <- sapply(n_seq, sup_power_calc)
    df_power <- data.frame(n1 = n_seq, Power = power_seq)
    
    # 6) 绘制图形：参考 Non-Inferiority 样式 -> 两条竖线: (功效=50%, 目标功效)，1条横线(目标功效)...
    output$sup_means_plot <- renderPlot({
      ggplot(df_power, aes(x = n1, y = Power)) +
        geom_line(color = "blue", linewidth = 1) +
        
        # 横线：目标功效
        geom_hline(yintercept = power, color = "red", linetype = "dashed") +
        
        # 竖线：功效=50%时(n1)，目标功效时(n1)
        geom_vline(xintercept = base_sample_size, color = "green",  linetype = "dashed") +
        geom_vline(xintercept = target_sample_size, color = "purple", linetype = "dashed") +
        
        ylim(0, 1) +
        labs(
          x = "Sample Size (n1)",
          y = "Power",
          title = "Superiority：n1 vs. Power"
        ) +
        theme_minimal(base_size = 14) +
        annotate("text", x = base_sample_size, y = 0.1,
                 label = "power=50%", color = "green", angle = 90, vjust = -0.5) +
        annotate("text", x = target_sample_size, y = 0.1,
                 label = "Target power", color = "purple", angle = 90, vjust = -0.5)
    })
  })
  
  
  
  
  

  ###────────────────────────────────────
  ### C.2 Two Proportions (Difference)----
  ###────────────────────────────────────
  observeEvent(input$sup_prop_calc, {
    # 1) 获取用户输入
    alpha <- input$sup_prop_alpha
    power <- input$sup_prop_power
    p1    <- input$sup_prop_p1
    p2    <- input$sup_prop_p2
    k     <- input$sup_prop_ratio
    beta  <- 1 - power
    
    # 2) 分别计算：功效=50% 与 目标功效的样本量
    base_sample_size <- TrialSize::TwoSampleProportion.Equality(
      alpha  = alpha,
      beta   = 0.50,  # => power=0.50
      p1     = p1,
      p2     = p2,
      k      = k
    )
    
    target_sample_size <- TrialSize::TwoSampleProportion.Equality(
      alpha  = alpha,
      beta   = beta,
      p1     = p1,
      p2     = p2,
      k      = k
    )
    
    # 3) 输出
    output$sup_prop_result <- renderPrint({
      cat("Sample size calculation for Superiority Test (Two Proportions - Difference):\n")
      cat("-------------------------------------------\n")
      cat(sprintf("Significance level (α): %.3f\n", alpha))
      cat(sprintf("Proportion in treatment group (p1): %.3f\n", p1))
      cat(sprintf("Proportion in control group (p2): %.3f\n", p2))
      cat(sprintf("Allocation ratio (k = n1/n2): %.3f\n", k))
      cat(sprintf("Target power: %.3f\n", power))
      cat("-------------------------------------------\n")
      cat(sprintf("Sample size (Power = 50%%): %.1f\n", base_sample_size))
      cat(sprintf("Sample size (Target power): %.1f\n", target_sample_size))
    })
    
  
  })
  ###────────────────────────────────────
  ### C.3 Two Proportions (Ratio)----
  ###────────────────────────────────────
  observeEvent(input$sup_ratio_calc, {
    # 1) 获取用户输入
    alpha <- input$sup_ratio_alpha
    power <- input$sup_ratio_power
    pt    <- input$sup_ratio_pt
    pc    <- input$sup_ratio_pc
    k     <- input$sup_ratio_alloc
    beta  <- 1 - power
    
    # 2) 计算 or_val
    or_val <- pt*(1-pc)/( pc*(1-pt) )
    
    # 3) 分别计算：功效 = 50% 与 目标功效的所需样本量
    base_sample_size <- TrialSize::RelativeRisk.Equality(
      alpha = alpha,
      beta  = 0.50,  # => power=0.50
      or    = or_val,
      k     = k,
      pt    = pt,
      pc    = pc
    )
    
    target_sample_size <- TrialSize::RelativeRisk.Equality(
      alpha = alpha,
      beta  = beta,
      or    = or_val,
      k     = k,
      pt    = pt,
      pc    = pc
    )
    
    # 4) 输出
    output$sup_ratio_result <- renderPrint({
      cat("Sample size calculation for Superiority Test (Two Proportions - Ratio):\n")
      cat("-------------------------------------------\n")
      cat(sprintf("Significance level (α): %.3f\n", alpha))
      cat(sprintf("Proportion in treatment group (p1): %.3f\n", pt))
      cat(sprintf("Proportion in control group (p2): %.3f\n", pc))
      cat(sprintf("Allocation ratio (k = n1/n2): %.3f\n", k))
      cat(sprintf("Calculated odds ratio: %.3f\n", or_val))
      cat(sprintf("Target power: %.3f\n", power))
      cat("-------------------------------------------\n")
      cat(sprintf("Sample size (Power = 50%%): %.1f\n", base_sample_size))
      cat(sprintf("Sample size (Target power): %.1f\n", target_sample_size))
    })
    
    

  })
  
  
  
  
  
  
  
  
  
 
  
  
  
  ####-###-###-###-###-###-###-###-###
  # (7) 生存分析类----
  ####-###-###-###-###-###-###-###-###
  
  ##===-===-===-===-===-===-===
  ##(8.1) Diagnostic Test Sample Size----
  ##===-===-===-===-===-===-===
  observeEvent(input$diag_calc, {
    # 获取用户输入
    diag_test <- input$diag_test        # 诊断测试性能 (敏感性或特异性)
    diag_type <- input$diag_type        # 计算类型 ("se" 或 "sp")
    diag_py <- input$diag_py            # 患病率
    diag_epsilon <- input$diag_epsilon  # 最大允许误差
    diag_alpha <- input$diag_alpha      # 显著性水平 (α)
    diag_power <- input$diag_power      # 功效目标值 (Power)
    
    # 调用 epiR::epi.ssdxsesp 计算样本量（满足目标值，功效为 50%）
    base_sample_size <- epi.ssdxsesp(
      test = diag_test,
      type = diag_type,
      Py = diag_py,
      epsilon = diag_epsilon,
      error = "absolute",               # 假设误差为绝对误差
      nfractional = FALSE,              # 返回整数样本量
      conf.level = 1 - diag_alpha       # 置信水平
    )
    
    # 计算目标功效值对应的样本量
    Z_alpha <- qnorm(1 - diag_alpha / 2)            # 显著性水平对应的 Z 值
    Z_beta <- qnorm(diag_power)                     # 功效目标值对应的 Z 值
    
    # 反推目标功效值对应的样本量
    target_sample_size <- ceiling(
      (Z_alpha + Z_beta)^2 * diag_test * (1 - diag_test) / 
        if (diag_type == "se") {
          (diag_epsilon^2 * diag_py)                # 敏感性公式
        } else {
          (diag_epsilon^2 * (1 - diag_py))          # 特异性公式
        }
    )
    
    # 输出计算结果
    output$diag_result <- renderPrint({
      cat("Diagnostic Test Sample Size Result:\n")
      cat("------------------------------------------\n")
      cat("Parameter estimated: ", ifelse(diag_type == "se", "Sensitivity (Se)", "Specificity (Sp)"), "\n")
      cat(sprintf("Target value (Se/Sp): %.3f\n", diag_test))
      cat(sprintf("Prevalence (P_y): %.3f\n", diag_py))
      cat(sprintf("Significance level (α): %.3f\n", diag_alpha))
      cat(sprintf("Margin of error (ε): %.3f\n", diag_epsilon))
      cat(sprintf("Target power: %.3f\n", diag_power))
      cat("\n")
      cat(sprintf("Required sample size (Power = 50%%): %d\n", base_sample_size))
      cat(sprintf("Required sample size (Target power): %d\n", target_sample_size))
    })
    
    
    # 功效曲线绘制逻辑
    n_seq <- seq(100, target_sample_size + 500, by = 100)  # 样本量范围
    
    # 根据样本量计算功效
    power_curve <- sapply(n_seq, function(n) {
      SE <- if (diag_type == "se") {
        sqrt(diag_test * (1 - diag_test) / (n * diag_py))              # 敏感性
      } else {
        sqrt(diag_test * (1 - diag_test) / (n * (1 - diag_py)))        # 特异性
      }
      EffectSize <- diag_epsilon / SE
      Power <- pnorm(EffectSize - Z_alpha)
      return(Power)
    })
    
    # 构造数据框用于绘图
    df_power <- data.frame(SampleSize = n_seq, Power = power_curve)
    
    # 绘制功效曲线
    output$diag_plot <- renderPlot({
      ggplot(df_power, aes(x = SampleSize, y = Power)) +
        geom_line(color = "blue", linewidth = 1) +  # 功效曲线
        geom_hline(yintercept = diag_power, color = "red", linetype = "dashed") +  # 功效目标线
        geom_vline(xintercept = base_sample_size, color = "green", linetype = "dashed") +  # 满足目标值样本量线
        geom_vline(xintercept = target_sample_size, color = "purple", linetype = "dashed") +  # 满足目标功效值样本量线
        ylim(0, 1) +
        labs(
          x = "Sample Size(n)",
          y = "Power",
          title = "Power Curve: Sample Size vs. Power"
          
        ) +
        theme_minimal(base_size = 14) +
        annotate("text", x = base_sample_size, y = 0.1, label = "power=50%", color = "green", angle = 90, vjust = -0.5) +
        annotate("text", x = target_sample_size, y = 0.1, label = "Target power", color = "purple", angle = 90, vjust = -0.5)
    })
  })
  
  ##===-===-===-===-===-===-===
  ##(7.2) Log-rank Test----
  ##===-===-===-===-===-===-===
  observeEvent(input$lr_calculate, {
    tryCatch({
      # 调用样本量计算函数
      lr_result <- TwoSampleSurvival.Equality(
        alpha = input$lr_alpha,
        beta = input$lr_beta,
        lam1 = input$lr_lam1,
        lam2 = input$lr_lam2,
        k = input$lr_k,
        ttotal = input$lr_ttotal,
        taccrual = input$lr_taccrual,
        gamma = input$lr_gamma
      )
      
      # 显示结果
      output$lr_result <- renderText({
        total_sample_size <- ceiling(lr_result)  # 总样本量
        n1 <- ceiling(total_sample_size * input$lr_k / (1 + input$lr_k))  # 对照组样本量
        n2 <- ceiling(total_sample_size / (1 + input$lr_k))  # 实验组样本量
        
        # 格式化输出
        paste0(
          "-----------------------------------------------\n",
          " Log-Rank Test (Two-Sample Survival Equality)\n",
          "-----------------------------------------------\n",
          " Total Sample Size       = ", total_sample_size, "\n",
          " Sample Size (Control)   = ", n1, "\n",
          " Sample Size (Treatment) = ", n2, "\n",
          " Significance Level      = ", input$lr_alpha, "\n",
          " Achieved Power          = ", 1 - input$lr_beta, "\n",
          " Hazard Rate (Control)   = ", input$lr_lam1, "\n",
          " Hazard Rate (Treatment) = ", input$lr_lam2, "\n",
          " Allocation Ratio (k)    = ", input$lr_k, "\n",
          " Alternative Hypothesis  = Two-Sided\n",
          "\nNOTE: Results are based on an exponential survival model.\n"
        )
      })
    }, error = function(e) {
      # 错误处理
      output$lr_result <- renderText({
        paste("计算失败: ", e$message)
      })
    })
  })
  
  ##===-===-===-===-===-===-===
  ##(7.3) Group Sequential (gsDesign)----
  ##===-===-===-===-===-===-===
  observeEvent(input$gsd_calc, {
    # 获取用户输入
    alpha   <- input$gsd_alpha      # 显著性水平
    power   <- input$gsd_power      # 检测效能
    n_looks <- input$gsd_nlooks     # 分析次数 (k)
    delta   <- input$gsd_delta      # 标准化效应大小
    sfu     <- input$gsd_sfu        # 上界花费函数
    sfl     <- input$gsd_sfl        # 下界花费函数
    
    # 计算 power = 1 - beta
    beta <- 1 - power
    
    # 计算组序设计
    res <- gsDesign::gsDesign(
      alpha = alpha,
      beta = beta,
      k = n_looks,
      delta = delta,
      test.type = 2,  # 双侧检验
      sfu = sfu,
      sfl = sfl
    )
    
    # 显示结果
    #output$gsd_result <- renderPrint({
      #cat("Group Sequential Design (gsDesign)\n\n")
      #print(res)
    #})
    # 显示简化后的结果
    output$gsd_result <- renderPrint({
      cat("Group Sequential Design Summary:\n")
      cat("-----------------------------------\n")
      cat(sprintf("Significance Level (α): %.3f\n", alpha))
      cat(sprintf("Power (1-β): %.3f\n", power))
      cat(sprintf("Number of Looks (k): %d\n", n_looks))
      cat(sprintf("Standardized Effect Size (δ): %.2f\n", delta))
      cat(sprintf("Upper Bound Spending Function: %s\n", sfu))
      cat(sprintf("Lower Bound Spending Function: %s\n", sfl))
      cat("-----------------------------------\n")
      cat(sprintf("Total Sample Size: %.1f\n", sum(res$n.I)))
    })
    
    # 绘制 gsDesign 的边界图
    output$gsd_plot <- renderPlot({
      plot(res, main = "Group Sequential Boundaries")
    })
  })
  
  ##===-===-===-===-===-===-===
  ## (7.4) Cox Proportional Hazards (TrialSize)----
  ##===-===-===-===-===-===-===
  
  
  observeEvent(input$cox_calc, {
    # 获取用户输入参数
    alpha <- input$cox_alpha       # 显著性水平
    power <- input$cox_power       # 检测效能
    hr    <- input$cox_hr          # 风险比
    p1    <- input$cox_p1          # 分配到组 1 的患者比例
    d     <- input$cox_d           # 观察到事件的概率
    
    # 计算 log(hazard ratio)
    loghr <- log(hr)
    
    # 计算 1 - beta
    beta <- 1 - power
    
    # 使用 TrialSize 包计算样本量
    res <- TrialSize::Cox.Equality(
      alpha = alpha,
      beta = beta,
      loghr = loghr,
      p1 = p1,
      d = d
    )
    
    # 提取结果
    total_sample_size <- res[1]  # 总样本量
    total_events <- res[2]       # 总事件数
    
    # 输出计算结果
    output$cox_result <- renderPrint({
      cat("Sample Size Calculation Result: Cox Proportional Hazards Model\n")
      cat("--------------------------------------------------------------\n")
      cat(sprintf("Significance Level (α): %.3f\n", alpha))
      cat(sprintf("Power: %.3f\n", power))
      cat(sprintf("Hazard Ratio (HR): %.3f\n", hr))
      cat(sprintf("Proportion in Group 1: %.3f\n", p1))
      cat(sprintf("Probability of Observing Event (d): %.3f\n", d))
      cat("\n")
      cat(sprintf("Required Total Sample Size: %.1f\n", total_sample_size))
    })
    
    
  })
  
  
}

#--------------- Shiny App ---------------
shinyApp(ui = ui, server = server)
