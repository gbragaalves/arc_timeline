# ---- Exportador Arc JSON (Timeline + Samples + samples_apagar) ----

exportar_arc_json <- function(timeline_items, samples, output_dir, data_trabalho) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  import_dir <- file.path(output_dir, "Import")
  ti_dir     <- file.path(import_dir, "TimelineItem")
  ls_dir     <- file.path(import_dir, "LocomotionSample")

  dir.create(ti_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(ls_dir, recursive = TRUE, showWarnings = FALSE)

  if (length(timeline_items) == 0 || length(samples) == 0) {
    return(invisible(NULL))
  }

  # index dos samples por sampleId
  sample_idx <- setNames(
    seq_along(samples),
    vapply(samples, `[[`, "", "sampleId")
  )

  now_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

  # ---- TimelineItem: um arquivo por item ----
  for (i in seq_along(timeline_items)) {
    it <- timeline_items[[i]]

    # nosso id interno
    internal_id <- it$.internalId %||% uuid::UUIDgenerate(use.time = TRUE)

    # garante start/end
    start_date <- it$startDate
    end_date   <- it$endDate

    # garante activityType razoável
    act_type <- it$activityType %||% if (isTRUE(it$.isVisit)) "stay" else "transport"

    # Se for airplane, marca como manual para forçar o Arc a aceitar
    manual_type <- (act_type == "airplane")

    # monta o objeto no formato esperado pelo Arc
    arc_item <- list(
      itemId             = internal_id,
      isVisit            = isTRUE(it$.isVisit),
      startDate          = start_date,
      endDate            = end_date,
      lastSaved          = now_utc,
      activityType       = act_type,
      manualActivityType = manual_type,
      altitude           = it$altitude %||% 0,
      stepCount          = 0,
      floorsAscended     = 0,
      floorsDescended    = 0
    )

    # se for visita e tiver place, podemos incluir
    if (isTRUE(it$.isVisit) && !is.null(it$place)) {
      arc_item$place <- it$place
    }

    # escreve um arquivo por TimelineItem
    ti_path <- file.path(ti_dir, paste0(internal_id, ".json"))
    jsonlite::write_json(
      arc_item,
      ti_path,
      auto_unbox = TRUE,
      pretty     = TRUE
    )

    # amarra os samples referenciados a este item
    if (!is.null(it$samples)) {
      for (sid in it$samples) {
        idx <- sample_idx[[sid]]
        if (!is.na(idx)) {
          samples[[idx]]$timelineItemId <- internal_id
        }
      }
    }
  }

  # ---- LocomotionSample: um arquivo único samples.json ----
  ls_path <- file.path(ls_dir, "samples.json")
  jsonlite::write_json(
    samples,
    ls_path,
    auto_unbox = TRUE,
    pretty     = TRUE
  )

  # ---- Samples a apagar (baseado nos arquivos semanais) ----

  apagar <- gerar_samples_apagar(timeline_items, semana_dir = SEMANA_DIR)

  if (length(apagar) > 0) {
    apagar_path <- file.path(ls_dir, "samples_apagar.json")
    jsonlite::write_json(
      apagar,
      apagar_path,
      auto_unbox = TRUE,
      pretty     = TRUE
    )
    message("samples_apagar.json criado com ", length(apagar), " samples para deletar")
  } else {
    message("Nenhum sample encontrado nos arquivos semanais para deletar")
    message("   Arquivos procurados em: ", SEMANA_DIR)
  }

  invisible(NULL)
}
