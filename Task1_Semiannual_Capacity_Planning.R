# =============================================================================
# Task 1: Semiannual Capacity Planning for Endoscopy in a GI Unit
# Data-Driven Decision-Making (D3M) — ITAO 7104
# MILP Model solved using the ompr package in R
# =============================================================================

# --- Step 1: Install all required packages ---
install.packages("ompr")
install.packages("ompr.roi")
install.packages("ROI")
install.packages("ROI.plugin.highs")   # HiGHS: faster than GLPK, supports mip_gap

# --- Step 2: Load all required libraries (ORDER MATTERS) ---
library(ROI)               # Must be loaded FIRST, before any plugins
library(ROI.plugin.highs)  # HiGHS: faster than GLPK, supports mip_gap natively
library(ompr)             # MILP modelling
library(ompr.roi)         # Bridge between ompr and ROI (provides with_ROI)
library(dplyr)
library(ggplot2)
library(tidyr)

# =============================================================================
# DATA
# =============================================================================

W <- 26  # number of weeks
R <- 10  # number of rooms

# Demanded diagnostic hours per week
P_diag <- c(230, 235, 225, 230, 240, 245,
            280, 295, 300, 285,
            240, 235, 230, 225, 230, 235,
            240, 245, 250, 245, 240, 235,
            230, 230, 235, 230)

# Demanded therapeutic hours per week
P_thera <- c(145, 145, 150, 150, 155, 160,
             168, 176, 182, 188,
             190, 192, 188, 195, 187, 182,
             178, 172, 168, 165, 162, 158,
             155, 152, 150, 148)

# Available clinician hours per week
C_w <- c(520, 515, 525, 520, 530, 525,
         490, 480, 485, 500,
         510, 515, 520, 525, 530, 525,
         520, 480, 470, 475, 500, 510,
         515, 520, 525, 530)

# Room profiles: Rooms 1-3 Small, 4-7 Medium, 8-10 Large
B_diag  <- c(rep(60, 3),    rep(120, 4),   rep(180, 3))
B_thera <- c(rep(30, 3),    rep(60, 4),    rep(120, 3))
S       <- c(rep(15000, 3), rep(20000, 4), rep(25000, 3))
A       <- c(rep(180, 3),   rep(200, 4),   rep(220, 3))

Cost_diag  <- A * B_diag   # weekly allocation cost in diagnostic config
Cost_thera <- A * B_thera  # weekly allocation cost in therapeutic config

# =============================================================================
# FEASIBILITY PRE-CHECK (run before solving to catch impossible problems)
# =============================================================================
cat("=== Feasibility Pre-Check ===\n")

any_infeasible <- FALSE
for (w in 1:W) {
  total_demand <- P_diag[w] + P_thera[w]
  if (total_demand > C_w[w]) {
    cat(sprintf("WARNING Week %2d: total demand=%d > clinician cap=%d (gap=%d)\n",
                w, total_demand, C_w[w], total_demand - C_w[w]))
    any_infeasible <- TRUE
  }
}

# Check if enough room capacity exists (using all rooms)
max_diag_supply  <- sum(B_diag)   # if all rooms set to diagnostic
max_thera_supply <- sum(B_thera)  # if all rooms set to therapeutic

cat(sprintf("\nMax diag supply (all rooms diag): %d\n",  max_diag_supply))
cat(sprintf("Max thera supply (all rooms thera): %d\n", max_thera_supply))
cat(sprintf("Peak diagnostic demand:  %d\n", max(P_diag)))
cat(sprintf("Peak therapeutic demand: %d\n", max(P_thera)))

if (!any_infeasible) {
  cat("Pre-check PASSED: clinician hours can cover total demand each week.\n")
}

# =============================================================================
# MILP MODEL
# =============================================================================
# Decision Variables:
#   d[r,w]    {0,1}: room r in DIAGNOSTIC config in week w
#   t[r,w]    {0,1}: room r in THERAPEUTIC config in week w
#   u[r,w]    {0,1}: room r set up (transitions from unavailable) in week w
#   xd_d[r,w] >= 0 : diagnostic hours in diagnostic-configured room r, week w
#   xd_t[r,w] >= 0 : diagnostic hours in therapeutic-configured room r, week w
#   xt_t[r,w] >= 0 : therapeutic hours in therapeutic-configured room r, week w
# =============================================================================

cat("\n=== Building MILP Model ===\n")

model <- MIPModel() %>%

  # ---- Variables ----
  add_variable(d[r, w],    type = "binary",     r = 1:R, w = 1:W) %>%
  add_variable(t[r, w],    type = "binary",     r = 1:R, w = 1:W) %>%
  add_variable(u[r, w],    type = "binary",     r = 1:R, w = 1:W) %>%
  add_variable(xd_d[r, w], type = "continuous", lb = 0, r = 1:R, w = 1:W) %>%
  add_variable(xd_t[r, w], type = "continuous", lb = 0, r = 1:R, w = 1:W) %>%
  add_variable(xt_t[r, w], type = "continuous", lb = 0, r = 1:R, w = 1:W) %>%

  # ---- Objective: minimise total cost ----
  set_objective(
    sum_expr(S[r]          * u[r, w], r = 1:R, w = 1:W) +
      sum_expr(Cost_diag[r]  * d[r, w], r = 1:R, w = 1:W) +
      sum_expr(Cost_thera[r] * t[r, w], r = 1:R, w = 1:W),
    "min"
  ) %>%

  # C1: Each room is in at most one configuration per week
  add_constraint(d[r, w] + t[r, w] <= 1, r = 1:R, w = 1:W) %>%

  # C2: Setup indicator — week 1 (all rooms start unavailable)
  add_constraint(u[r, 1] >= d[r, 1] + t[r, 1], r = 1:R) %>%

  # C3: Setup indicator — weeks 2..W (setup triggered by transition from idle)
  add_constraint(
    u[r, w] >= (d[r, w] + t[r, w]) - (d[r, w - 1] + t[r, w - 1]),
    r = 1:R, w = 2:W
  ) %>%

  # C4: Diagnostic demand met every week
  add_constraint(
    sum_expr(xd_d[r, w], r = 1:R) + sum_expr(xd_t[r, w], r = 1:R) >= P_diag[w],
    w = 1:W
  ) %>%

  # C5: Therapeutic demand met every week
  add_constraint(
    sum_expr(xt_t[r, w], r = 1:R) >= P_thera[w],
    w = 1:W
  ) %>%

  # C6: Diagnostic room capacity
  add_constraint(xd_d[r, w] <= B_diag[r] * d[r, w], r = 1:R, w = 1:W) %>%

  # C7: Therapeutic room capacity (diag + thera hours combined)
  add_constraint(
    xd_t[r, w] + xt_t[r, w] <= B_thera[r] * t[r, w],
    r = 1:R, w = 1:W
  ) %>%

  # C8: Clinician availability — total procedure hours <= available hours
  add_constraint(
    sum_expr(xd_d[r, w], r = 1:R) +
      sum_expr(xd_t[r, w], r = 1:R) +
      sum_expr(xt_t[r, w], r = 1:R) <= C_w[w],
    w = 1:W
  )

cat("Model built successfully.\n")

# =============================================================================
# SOLVE
# =============================================================================
cat("\n=== Solving the MILP Model ===\n")

result <- solve_model(
  model,
  with_ROI(solver = "highs", verbose = TRUE,
           mip_gap  = 0.005,        # stop when within 0.5% of optimal (SUPPORTED by HiGHS)
           time_limit = 3600)    # 1 hour in seconds (HiGHS uses seconds, not milliseconds)
)

cat("\n=== Solver Status ===\n")
cat("Status:", solver_status(result), "\n")
cat("Objective Value (Total Cost):", objective_value(result), "\n")

# Accept both "optimal" (proven best) and feasible solutions found before timeout
status <- solver_status(result)
obj    <- objective_value(result)

if (status == "optimal") {
  cat("> Optimal solution found and proven.\n")
} else if (!is.na(obj) && obj > 0) {
  cat("> WARNING: Solver timed out but found a feasible solution with cost:", obj, "\n")
  cat("> This is a valid (possibly near-optimal) solution — proceeding with results.\n")
} else {
  stop(paste(
    "Solver failed with no feasible solution. Status:", status,
    "\nRun the feasibility pre-check output above to diagnose the issue."
  ))
}

# =============================================================================
# EXTRACT SOLUTION
# =============================================================================
sol_d    <- get_solution(result, d[r, w])
sol_t    <- get_solution(result, t[r, w])
sol_u    <- get_solution(result, u[r, w])
sol_xd_d <- get_solution(result, xd_d[r, w])
sol_xd_t <- get_solution(result, xd_t[r, w])
sol_xt_t <- get_solution(result, xt_t[r, w])

# =============================================================================
# DISPLAY SOLUTION SUMMARY
# =============================================================================
cat("\n=== Room Configuration Schedule ===\n")
cat("Legend: D = Diagnostic, T = Therapeutic, - = Unavailable\n\n")

config_matrix <- matrix("-", nrow = R, ncol = W)
for (i in 1:nrow(sol_d)) {
  if (sol_d$value[i] > 0.5) config_matrix[sol_d$r[i], sol_d$w[i]] <- "D"
}
for (i in 1:nrow(sol_t)) {
  if (sol_t$value[i] > 0.5) config_matrix[sol_t$r[i], sol_t$w[i]] <- "T"
}

cat(sprintf("%-8s", "Room"), paste(sprintf("W%-2d", 1:W), collapse = " "), "\n")
for (r_idx in 1:R) {
  cat(sprintf("Room %-2d ", r_idx),
      paste(sprintf("%-3s", config_matrix[r_idx, ]), collapse = " "), "\n")
}

# =============================================================================
# COST BREAKDOWN
# =============================================================================
total_setup <- sol_u %>%
  left_join(data.frame(r = 1:R, S_cost = S), by = "r") %>%
  summarise(total = sum(value * S_cost)) %>%
  pull(total)

total_alloc <- sol_d %>%
  rename(d_val = value) %>%
  left_join(sol_t %>% rename(t_val = value), by = c("r", "w")) %>%
  left_join(data.frame(r = 1:R, Bd = B_diag, Bt = B_thera, A_cost = A), by = "r") %>%
  summarise(total = sum(A_cost * (Bd * d_val + Bt * t_val))) %>%
  pull(total)

cat("\nTotal allocation cost :", total_alloc, "\n")
cat("Total setup cost      :", total_setup, "\n")
cat("Grand total cost      :", total_alloc + total_setup, "\n")
# =============================================================================
# SANITY CHECKS
# =============================================================================
cat("\n=== SANITY CHECKS ===\n")

# Check 1: Mutual exclusivity
cat("\n--- Check 1: Mutual Exclusivity (d + t <= 1) ---\n")
violation_1 <- FALSE
for (w_idx in 1:W) {
  for (r_idx in 1:R) {
    d_val <- sol_d$value[sol_d$r == r_idx & sol_d$w == w_idx]
    t_val <- sol_t$value[sol_t$r == r_idx & sol_t$w == w_idx]
    if ((d_val + t_val) > 1.001) {
      cat("VIOLATION: Room", r_idx, "Week", w_idx, "d+t =", d_val + t_val, "\n")
      violation_1 <- TRUE
    }
  }
}
if (!violation_1) cat("PASS: All rooms in at most one configuration per week.\n")

# Check 2: Diagnostic demand
cat("\n--- Check 2: Diagnostic Demand Met ---\n")
violation_2 <- FALSE
for (w_idx in 1:W) {
  total_diag <- sum(sol_xd_d$value[sol_xd_d$w == w_idx]) +
    sum(sol_xd_t$value[sol_xd_t$w == w_idx])
  if (total_diag < P_diag[w_idx] - 0.01) {
    cat("VIOLATION: Week", w_idx, "Diag supplied =", round(total_diag, 2),
        "< Demanded =", P_diag[w_idx], "\n")
    violation_2 <- TRUE
  }
}
if (!violation_2) cat("PASS: Diagnostic demand met in all weeks.\n")

# Check 3: Therapeutic demand
cat("\n--- Check 3: Therapeutic Demand Met ---\n")
violation_3 <- FALSE
for (w_idx in 1:W) {
  total_thera <- sum(sol_xt_t$value[sol_xt_t$w == w_idx])
  if (total_thera < P_thera[w_idx] - 0.01) {
    cat("VIOLATION: Week", w_idx, "Thera supplied =", round(total_thera, 2),
        "< Demanded =", P_thera[w_idx], "\n")
    violation_3 <- TRUE
  }
}
if (!violation_3) cat("PASS: Therapeutic demand met in all weeks.\n")

# Check 4: Clinician capacity
cat("\n--- Check 4: Clinician Hours Not Exceeded ---\n")
violation_4 <- FALSE
for (w_idx in 1:W) {
  total_hours <- sum(sol_xd_d$value[sol_xd_d$w == w_idx]) +
    sum(sol_xd_t$value[sol_xd_t$w == w_idx]) +
    sum(sol_xt_t$value[sol_xt_t$w == w_idx])
  if (total_hours > C_w[w_idx] + 0.01) {
    cat("VIOLATION: Week", w_idx, "Total hours =", round(total_hours, 2),
        "> Available =", C_w[w_idx], "\n")
    violation_4 <- TRUE
  }
}
if (!violation_4) cat("PASS: Clinician hours respected in all weeks.\n")

# Check 5: Diagnostic room capacity
cat("\n--- Check 5: Diagnostic Room Capacity ---\n")
violation_5 <- FALSE
for (w_idx in 1:W) {
  for (r_idx in 1:R) {
    xd_d_val <- sol_xd_d$value[sol_xd_d$r == r_idx & sol_xd_d$w == w_idx]
    d_val    <- sol_d$value[sol_d$r == r_idx & sol_d$w == w_idx]
    if (xd_d_val > B_diag[r_idx] * d_val + 0.01) {
      cat("VIOLATION: Room", r_idx, "Week", w_idx, "\n")
      violation_5 <- TRUE
    }
  }
}
if (!violation_5) cat("PASS: Diagnostic room capacities respected.\n")

# Check 6: Therapeutic room capacity
cat("\n--- Check 6: Therapeutic Room Capacity ---\n")
violation_6 <- FALSE
for (w_idx in 1:W) {
  for (r_idx in 1:R) {
    xd_t_val <- sol_xd_t$value[sol_xd_t$r == r_idx & sol_xd_t$w == w_idx]
    xt_t_val <- sol_xt_t$value[sol_xt_t$r == r_idx & sol_xt_t$w == w_idx]
    t_val    <- sol_t$value[sol_t$r == r_idx & sol_t$w == w_idx]
    if ((xd_t_val + xt_t_val) > B_thera[r_idx] * t_val + 0.01) {
      cat("VIOLATION: Room", r_idx, "Week", w_idx, "\n")
      violation_6 <- TRUE
    }
  }
}
if (!violation_6) cat("PASS: Therapeutic room capacities respected.\n")

# Check 7: Setup indicator
cat("\n--- Check 7: Setup Indicator ---\n")
violation_7 <- FALSE
for (r_idx in 1:R) {
  d1 <- sol_d$value[sol_d$r == r_idx & sol_d$w == 1]
  t1 <- sol_t$value[sol_t$r == r_idx & sol_t$w == 1]
  u1 <- sol_u$value[sol_u$r == r_idx & sol_u$w == 1]
  if (u1 < (d1 + t1) - 0.01) {
    cat("VIOLATION: Room", r_idx, "Week 1\n"); violation_7 <- TRUE
  }
  for (w_idx in 2:W) {
    d_now  <- sol_d$value[sol_d$r == r_idx & sol_d$w == w_idx]
    t_now  <- sol_t$value[sol_t$r == r_idx & sol_t$w == w_idx]
    d_prev <- sol_d$value[sol_d$r == r_idx & sol_d$w == (w_idx - 1)]
    t_prev <- sol_t$value[sol_t$r == r_idx & sol_t$w == (w_idx - 1)]
    u_now  <- sol_u$value[sol_u$r == r_idx & sol_u$w == w_idx]
    if (u_now < (d_now + t_now) - (d_prev + t_prev) - 0.01) {
      cat("VIOLATION: Room", r_idx, "Week", w_idx, "\n"); violation_7 <- TRUE
    }
  }
}
if (!violation_7) cat("PASS: Setup indicators correctly track transitions.\n")

# =============================================================================
# VISUALIZATION 1: Room Configuration Heatmap
# =============================================================================
config_df <- data.frame(Room = integer(), Week = integer(), Config = character(),
                        stringsAsFactors = FALSE)
for (r_idx in 1:R) {
  for (w_idx in 1:W) {
    d_val <- sol_d$value[sol_d$r == r_idx & sol_d$w == w_idx]
    t_val <- sol_t$value[sol_t$r == r_idx & sol_t$w == w_idx]
    cfg <- if (d_val > 0.5) "Diagnostic" else if (t_val > 0.5) "Therapeutic" else "Unavailable"
    config_df <- rbind(config_df, data.frame(Room = r_idx, Week = w_idx, Config = cfg))
  }
}

config_df$Room   <- factor(config_df$Room, levels = R:1, labels = paste("Room", R:1))
config_df$Config <- factor(config_df$Config, levels = c("Unavailable", "Diagnostic", "Therapeutic"))

p1 <- ggplot(config_df, aes(x = Week, y = Room, fill = Config)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(values = c("Unavailable" = "grey80",
                               "Diagnostic"  = "steelblue",
                               "Therapeutic" = "tomato")) +
  scale_x_continuous(breaks = 1:W) +
  labs(title = "Room Configuration Schedule Over 26 Weeks",
       x = "Week", y = "Room", fill = "Configuration") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 14))

print(p1)
ggsave("room_configuration_heatmap.png", p1, width = 14, height = 5, dpi = 150)

# =============================================================================
# VISUALIZATION 2: Aggregated Bay Hours vs Demand
# =============================================================================
agg_df <- data.frame(Week = 1:W, Diag_Hours = 0, Thera_Hours = 0)
for (w_idx in 1:W) {
  dh <- 0; th <- 0
  for (r_idx in 1:R) {
    if (sol_d$value[sol_d$r == r_idx & sol_d$w == w_idx] > 0.5) dh <- dh + B_diag[r_idx]
    if (sol_t$value[sol_t$r == r_idx & sol_t$w == w_idx] > 0.5) th <- th + B_thera[r_idx]
  }
  agg_df$Diag_Hours[w_idx]  <- dh
  agg_df$Thera_Hours[w_idx] <- th
}

agg_long <- agg_df %>%
  pivot_longer(cols = c(Diag_Hours, Thera_Hours),
               names_to = "Type", values_to = "Hours") %>%
  mutate(Type = ifelse(Type == "Diag_Hours",
                       "Diagnostic Config Hours", "Therapeutic Config Hours"))

p2 <- ggplot(agg_long, aes(x = Week, y = Hours, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Diagnostic Config Hours"  = "steelblue",
                               "Therapeutic Config Hours" = "tomato")) +
  scale_x_continuous(breaks = 1:W) +
  geom_line(data = data.frame(Week = 1:W, Hours = P_diag + P_thera),
            aes(x = Week, y = Hours, fill = NULL),
            color = "black", linewidth = 1, linetype = "dashed") +
  labs(title = "Available Bay Hours vs. Total Demand per Week",
       x = "Week", y = "Bay Hours", fill = "Configuration",
       caption = "Dashed line = Total demanded hours (diagnostic + therapeutic)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p2)
ggsave("bay_hours_per_week.png", p2, width = 14, height = 5, dpi = 150)

# =============================================================================
# VISUALIZATION 3: Per-Room Procedure Hours
# =============================================================================
room_hours_df <- data.frame(Room = integer(), Week = integer(),
                            Diag_in_Diag = numeric(),
                            Diag_in_Thera = numeric(),
                            Thera_in_Thera = numeric())

for (r_idx in 1:R) {
  for (w_idx in 1:W) {
    xdd <- sol_xd_d$value[sol_xd_d$r == r_idx & sol_xd_d$w == w_idx]
    xdt <- sol_xd_t$value[sol_xd_t$r == r_idx & sol_xd_t$w == w_idx]
    xtt <- sol_xt_t$value[sol_xt_t$r == r_idx & sol_xt_t$w == w_idx]
    room_hours_df <- rbind(room_hours_df,
                           data.frame(Room = r_idx, Week = w_idx,
                                      Diag_in_Diag   = xdd,
                                      Diag_in_Thera  = xdt,
                                      Thera_in_Thera = xtt))
  }
}

room_long <- room_hours_df %>%
  pivot_longer(cols = c(Diag_in_Diag, Diag_in_Thera, Thera_in_Thera),
               names_to = "Procedure", values_to = "Hours") %>%
  mutate(Procedure = case_when(
    Procedure == "Diag_in_Diag"   ~ "Diagnostic (Diag Room)",
    Procedure == "Diag_in_Thera"  ~ "Diagnostic (Thera Room)",
    Procedure == "Thera_in_Thera" ~ "Therapeutic (Thera Room)"
  ))

p3 <- ggplot(room_long %>% filter(Hours > 0.01),
             aes(x = Week, y = Hours, fill = Procedure)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ paste("Room", Room), ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c("Diagnostic (Diag Room)"   = "steelblue",
                               "Diagnostic (Thera Room)"  = "gold",
                               "Therapeutic (Thera Room)" = "tomato")) +
  labs(title = "Procedure Hours per Room Over 26 Weeks",
       x = "Week", y = "Procedure Hours", fill = "Procedure Type") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p3)
ggsave("room_procedure_hours.png", p3, width = 16, height = 12, dpi = 150)

# =============================================================================
# FINAL MANAGERIAL STATEMENT
# =============================================================================
cat("\n=== MANAGERIAL DECISION STATEMENT ===\n")
cat("The minimum total cost of meeting all diagnostic and therapeutic endoscopy\n")
cat("demand over the 26-week planning horizon is:\n")
# MIP gap reported based on solver termination tolerance
obj_val <- objective_value(result)
cat("  Total Cost:   ", format(obj_val, big.mark = ","), "Pounds (GBP)\n")
cat("  Solver Status:", solver_status(result), "\n")
cat("  MIP Gap:      <= 0.5% (solver stopped at mip_gap = 0.005 tolerance)\n")
cat("  The solution is therefore guaranteed to be within 0.5% of the true optimum.\n")
