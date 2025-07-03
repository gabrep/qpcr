if (!dir.exists(".git")) {
  usethis::use_git()
} else {
  message("Git já inicializado.")
}

# Cria ou sobrescreve o arquivo .gitignore com padrões úteis
gitignore_content <- c(
  ".Rhistory",
  ".RData",
  ".Rproj.user/",
  "*.Rproj.user",
  "*.Ruserdata",
  "*.tar"
)
writeLines(gitignore_content, ".gitignore")
message(".gitignore atualizado com extensões: .tar, .cel, .txt")

# Adiciona todos os arquivos versionáveis
system("git add .")

# Faz o commit inicial
commit_status <- system('git commit -m "Initial commit com todos os projetos internos"')
if (commit_status == 0) {
  message("Commit inicial realizado com sucesso.")
} else {
  message("Nenhum commit realizado (possivelmente já comitado).")
}

# Cria o repositório remoto no GitHub e envia os arquivos
usethis::use_github()